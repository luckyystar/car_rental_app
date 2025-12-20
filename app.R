source("packages.R")

# -------------------------- DB CONNECTION --------------------------
# SQLite database file in your project folder
con <- dbConnect(
  RSQLite::SQLite(),
  "car_rental.sqlite"
)

# Ensure foreign keys are enforced
dbExecute(con, "PRAGMA foreign_keys = ON;")

# -------------------------- CREATE TABLES --------------------------

# Customers table
dbExecute(con, "
CREATE TABLE IF NOT EXISTS customers (
  customer_id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT NOT NULL,
  contact TEXT NOT NULL,
  email TEXT NOT NULL UNIQUE,
  updated_at TEXT DEFAULT (datetime('now'))
)
")

# Cars table
dbExecute(con, "
CREATE TABLE IF NOT EXISTS cars (
  car_id INTEGER PRIMARY KEY AUTOINCREMENT,
  brand TEXT NOT NULL,
  model TEXT NOT NULL,
  year INTEGER NOT NULL,
  type TEXT NOT NULL,
  price_per_day REAL NOT NULL,
  status TEXT DEFAULT 'available',
  image TEXT,
  updated_at TEXT DEFAULT (datetime('now'))
)
")

# Users table
dbExecute(con, "
CREATE TABLE IF NOT EXISTS users (
  user_id INTEGER PRIMARY KEY AUTOINCREMENT,
  email TEXT NOT NULL UNIQUE,
  password_hash TEXT NOT NULL,
  role TEXT DEFAULT 'admin',
  created_at TEXT DEFAULT (datetime('now'))
)
")

# Bookings table
dbExecute(con, "
CREATE TABLE IF NOT EXISTS bookings (
  booking_id INTEGER PRIMARY KEY AUTOINCREMENT,
  customer_id INTEGER NOT NULL,
  car_id INTEGER NOT NULL,
  start_date TEXT NOT NULL,
  end_date TEXT NOT NULL,
  total_amount REAL NOT NULL,
  updated_at TEXT DEFAULT (datetime('now')),
  status TEXT CHECK(status IN ('reserved','ongoing','ended')) NOT NULL,
  FOREIGN KEY (customer_id) REFERENCES customers(customer_id) ON DELETE CASCADE,
  FOREIGN KEY (car_id) REFERENCES cars(car_id) ON DELETE CASCADE
)
")

# -------------------------- SAFE QUERY FUNCTION --------------------------
safe_query <- function(q) {
  tryCatch(
    dbGetQuery(con, q),
    error = function(e) {
      message("DB query error: ", e$message)
      data.frame()
    }
  )
}

# -------------------------- UI --------------------------

ui <- uiOutput("app_ui")  

# -------------------------- SERVER --------------------------
server <- function(input, output, session) {
  # ---- Reactive login state ----
  user_logged_in <- reactiveVal(FALSE)
  current_user <- reactiveVal(NULL)
  cars_per_page <- 8
  current_page <- reactiveVal(1)
  
  # ---- LOGIN PAGE ----
  login_page <- fluidPage(
    style = "padding:0; margin:0;",
    
    tags$head(
      
      tags$link(
        rel = "stylesheet",
        href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.1/css/all.min.css"
      ),
      
      tags$style(HTML("
    /* ===== LOGIN PAGE ===== */
    .login-page {
        min-height: 100vh;
        background-color: #F8FAFC;
        display: flex;
        align-items: center;
        justify-content: center;
        font-family: 'Inter', sans-serif;
        padding: 0 !important;
        margin: 0;
        position: relative; /* needed for bubbles */
      }
      
      .login-container {
        position: relative; /* make login above bubbles */
        z-index: 1;
      }

    .login-page .login-container { 
      width: 100%;
      max-width: 1200px;
      min-height: 660px;
      box-shadow: 0 20px 50px rgba(0,0,0,0.12);
      border-radius: 18px;
      overflow: hidden;
      display: flex;
      background: #FFFFFF;
      flex-wrap: wrap; /* Allow responsive stacking */
    }
    
    /* LEFT & RIGHT */
    .login-page .login-left,
    .login-page .login-right {
      flex: 1 1 50%;
      min-width: 300px; /* Prevent being too small */
    }
    
    .login-page .login-left {
      background-image: url('uploads/login.png');
      background-size: cover;
      background-position: center;
    }
    
    /* RIGHT SIDE */
    .login-page .login-right {
      padding: 40px 30px; /* Reduced padding for responsiveness */
      display: flex;
      flex-direction: column;
      justify-content: center;
      align-items: center;
      box-sizing: border-box;
    }
    
    /* LOGO */
    .login-page .login-logo {
      max-width: 5200px;
      width: 100%;
      margin-bottom: 20px;
    }
    
    /* TITLE & TEXT */
    .login-page h2 {
      color: #0B1F5B;  /* deep blue */
      font-weight: 800;
      font-size: 28px;
      margin-bottom: 6px;
      text-align: center;
    }
    
    .login-page p {
      text-align: center;
      color: #6B7280;
      margin-bottom: 20px;
      font-size: 14px;
    }
    
    /* FORM WRAPPER */
    .login-page .login-form {
      width: 100%;
      max-width: 560px;
    }
    
    /* Labels */
    .login-page .login-label {
      font-size: 13px;
      font-weight: 600;
      color: #374151;
      margin-bottom: 6px;
      display: block;
    }
    
    /* Form inputs */
    .login-page .login-form .form-group {
      width: 100%;
      margin-bottom: 16px;
    }
    
    .login-page .login-form input {
      width: 100% !important;
      height: 48px;
      padding: 10px 14px;
      font-size: 14px;
      border-radius: 10px;
      border: 1px solid #D1D5DB;
    }
    
    /* BUTTON */
    .login-page .btn-login {
      width: 100% !important;
      height: 50px;
      background-color: #F59E0B;
      color: #FFFFFF;
      border-radius: 12px;
      font-weight: 800;
      font-size: 15px;
      border: none;
      margin-top: 8px;
    }
    
    /* MESSAGE */
    .login-page .login-msg {
      margin-top: 18px;
      color: #DC2626;
      font-weight: 600;
      text-align: center;
    }
    
    /* RESPONSIVE */
    @media (max-width: 768px) {
      .login-page .login-container {
        flex-direction: column;
        min-height: 600px;
      }
      .login-page .login-left,
      .login-page .login-right {
        width: 100%;
      }
      .login-page .login-right {
        padding: 30px 20px;
      }
    }
    
    /* ===== CAR BUBBLES ANIMATION ===== */
    .car-bubbles {
      position: absolute;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
      pointer-events: none; 
      overflow: hidden;
      z-index: 0;
    }
    
    .car-bubbles div {
      position: absolute;
      bottom: -50px; 
      width: 201px;
      height: 200px;
      background-image: url('uploads/car_icon.png'); 
      background-size: contain;
      background-repeat: no-repeat;
      opacity: 0;            
      animation: rise 10s linear infinite;
    }
    
    @keyframes riseRotate {
      0% {
        transform: translateY(0) scale(0.5) rotate(0deg);
        opacity: 0;
      }
      10% {
        opacity: 0.7;
      }
      100% {
        transform: translateY(-110vh) scale(1) rotate(360deg);
        opacity: 0;
      }
    }
    
    /* ===== PASSWORD TOGGLE ===== */
.password-wrapper {
  position: relative;
  width: 100%;
}

.password-wrapper input {
  padding-right: 42px !important; /* space for eye */
}

.toggle-password {
  position: absolute;
  top: 50%;
  right: 14px;
  transform: translateY(-50%);
  cursor: pointer;
  color: #6B7280;
  font-size: 16px;
}

.toggle-password:hover {
  color: #111827;
}

    
    ")),
      tags$script(HTML("
      // Trigger login on Enter key
      $(document).on('keydown', '.login-form input', function(e) {
        if(e.key === 'Enter') {
          e.preventDefault(); // prevent default Enter behavior
          $('#login_btn').click();
        }
      });

      // Generate car bubbles animation
      $(document).ready(function(){
        const container = $('.car-bubbles');
        const bubbleCount = 40; // number of bubbles
      
        for(let i = 0; i < bubbleCount; i++){
          const car = $('<div></div>');
          
          const leftPos = Math.random() * 100;          
          const startBottom = -50;                     
          const duration = 8 + Math.random() * 5;       
          const size = 200 + Math.random() * 50;         
          const delay = Math.random() * 5;              
      
          const rotateDeg = Math.random() < 0.5 ? 360 : -360; 
  
          car.css({
            left: leftPos + '%',
            bottom: startBottom + 'px',
            width: size + 'px',
            height: size + 'px',
            animationDuration: duration + 's',
            animationDelay: delay + 's',
            animationName: 'riseRotate'
          });
  
      
          container.append(car);
        }
      });
      
      // Toggle password visibility
$(document).on('click', '.toggle-password', function () {
  const input = $('#login_pass');
  const icon = $(this).find('i');

  if (input.attr('type') === 'password') {
    input.attr('type', 'text');
    icon.removeClass('fa-eye-slash').addClass('fa-eye');
  } else {
    input.attr('type', 'password');
    icon.removeClass('fa-eye').addClass('fa-eye-slash');
  }
});

    "))
    ),
    
    div(
      class = "login-page",
      div(class = "car-bubbles"),
      
      div(
        class = "login-container",
        div(class = "login-left"),
        div(
          class = "login-right",
          
          # LOGO
          tags$img(
            src = "uploads/logo.png",
            class = "login-logo"
          ),
          p("Log in to continue using the car rental service."),
          
          div(
            class = "login-form",
            
            tags$label("Email Address", class = "login-label"),
            textInput("login_email", NULL, placeholder = "Enter your email"),
            
            tags$label("Password", class = "login-label"),
            div(
              class = "password-wrapper",
              passwordInput(
                "login_pass",
                NULL,
                placeholder = "Enter your password"
              ),
              tags$span(
                class = "toggle-password",
                tags$i(class = "fa fa-eye-slash")
              )
            ),
            
            actionButton("login_btn", "Login", class = "btn-login"),
            
            div(class = "login-msg", textOutput("login_msg"))
          )
        )
      )
    )
  )
  
  
  
  # ---- LOGIN ACTION ----
  observeEvent(input$login_btn, {
    req(input$login_email, input$login_pass)
    
    user <- safe_query(paste0(
      "SELECT * FROM users WHERE email = ",
      DBI::dbQuoteString(con, input$login_email)
    ))
    
    if (nrow(user) == 0) {
      output$login_msg <- renderText("Invalid email or password.")
      return()
    }
    
    entered_hash <- digest(input$login_pass, algo = "sha256")
    stored_hash  <- trimws(user$password_hash)
    
    if (entered_hash == stored_hash) {
      user_logged_in(TRUE)
      current_user(user)
      output$login_msg <- renderText("")
    } else {
      output$login_msg <- renderText("Invalid email or password.")
    }
  })
  
  observeEvent(input$logout_btn, {
    user_logged_in(FALSE)
    current_user(NULL)
  })
  
  observeEvent(user_logged_in(), {
    if (user_logged_in()) {
      # Wait until the UI is rendered, then force dashboard tab
      session$onFlushed(function() {
        updateTabItems(session, "tabs", "dashboard")
      }, once = TRUE)
    }
  })
  
  output$app_ui <- renderUI({
    if (!user_logged_in()) {
      # Show login page if user is not logged in
      login_page
    } else {
      # Dashboard UI when logged in
      dashboardPage(
        skin = "blue",
        dashboardHeader(
          title = tagList(
            tags$div(
              style = "display: flex; align-items: center; margin-left: -15px;",
              tags$img(src = "uploads/logo.png", height = "40px", style = "margin-right: 10px; margin-top:5px;"),
              span(uiOutput("page_title_ui"), style = "font-weight:700; font-size:22px; color:#334155;")
            )
          ),
          titleWidth = 230,
          tags$li(
            class = "dropdown",
            style = "padding: 10px;",
            actionButton("logout_btn", "Logout", icon = icon("sign-out-alt"), 
                         style = "background-color:#F59E0B; color:white; font-weight:700; border:none;")
          )
        ),
        dashboardSidebar(
          div(
            class = "sidebar-user-panel",
            tags$div(class = "user-icon", icon("user")),
            tags$div(class = "user-name", "crsadmin"),
            tags$div(class = "user-role", "administrator")
          ),
          
          # Main menu items
          sidebarMenu(
            id = "tabs",
            menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer-alt"), selected = TRUE),
            menuItem("Cars", tabName = "cars", icon = icon("car")),
            menuItem("Bookings", tabName = "booking", icon = icon("calendar-check")),
            menuItem("Customers", tabName = "customers", icon = icon("users"))
          ),
          
          width = 230
        ),
        dashboardBody(
          tags$head(
            tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap');
      
      body,
      h1, h2, h3, h4, h5, h6,
      .box,
      .box-title,
      .small-box,
      .kpi-card,
      .leaderboard-card,
      .dataTable,
      label,
      input,
      select,
      textarea,
      button {
        font-family: 'Inter', sans-serif !important;
      }

    /* ===== HEADER ===== */
    .skin-blue .main-header .logo {
      background-color: #FCD34D !important;
      color: #1F2937 !important;
      font-weight: 700;
    }
    .skin-blue .main-header .logo:hover {
      background-color: #FCD34D !important;
      color: #1F2937 !important;
    }
    
    .skin-blue .main-header .navbar {
      background-color: #FCD34D !important;
    }
    
    
    /* Hamburger */
    .sidebar-toggle {
      color: #1D4ED8 !important;  /* Blue color */
    }
    .skin-blue .main-header .navbar .sidebar-toggle:hover {
      background-color: rgba(0,0,0,0.05) !important;
    }
    
    .main-header .logo {
      background-color: #FFFFFF !important;  
      color: #1E3A8A !important;            
      font-weight: 700;
    }
    
     /* ===== SIDEBAR ===== */
     
    .main-sidebar {
        min-height: 100vh !important;  
        position: fixed;               /* Keep it fixed on scroll */
        overflow-y: auto;              /* Scroll if content overflows */
    }
    
    .content-wrapper {
        margin-left: 230px; 
        min-height: 100vh;
        background-color: #F9FAFB; 
    }
    
    .main-sidebar,
    .main-sidebar .sidebar,
    .left-side {
      background-color: #1E3A8A !important;
    }
    
    .sidebar-menu > li > a {
      color: #E5E7EB !important;
      font-weight: 600;
    }
    
    .sidebar-menu > li > a:hover {
      background-color: #1D4ED8 !important;
      color: #FFFFFF !important;
    }
    
    .sidebar-menu > li.active > a {
      background-color: #1D4ED8 !important;
      color: #FFFFFF !important;
    }
    
    .sidebar-menu > li > a > .fa,
    .sidebar-menu > li > a > .fas {
      color: #E5E7EB !important;
    }
    
    .sidebar-menu > li.active > a > .fa,
    .sidebar-menu > li.active > a > .fas {
      color: #FFFFFF !important;
    }
    
    /* Desktop */
    @media (min-width: 768px) {
      /* Normal sidebar */
      .content-wrapper,
      .right-side {
        margin-left: 230px;
      }
    
      body.sidebar-collapse .content-wrapper,
      body.sidebar-collapse .right-side {
        margin-left: 0 !important;
      }
    
      body.sidebar-collapse .main-sidebar {
        width: 0 !important;
        overflow: hidden;
      }
    }
    
    /* Mobile: sidebar slides over content automatically, remove left margin */
    @media (max-width: 767px) {
      .content-wrapper,
      .right-side {
        margin-left: 0 !important;
      }
    
      .main-sidebar,
      .skin-blue .main-sidebar {
        min-height: 100vh !important;
      }
      
      .main-header .logo {
        display: flex !important;
        justify-content: center !important;  
        align-items: center !important;      
        width: 100% !important;              
        margin-left: 0 !important;           
      }
    
      .main-header .logo span {
        display: none;
      }
    
      .main-header .logo img {
        height: 40px !important;
        width: auto !important;
      }
    
    }


    /* ===== SIDEBAR USER PANEL ===== */
    .sidebar-user-panel {
      text-align: center;
      padding: 20px 10px;
      border-bottom: 1px solid rgba(255,255,255,0.15);
    }
    
    .user-icon {
      width: 56px;
      height: 56px;
      border-radius: 50%;
      background: #1D4ED8;
      display: flex;
      align-items: center;
      justify-content: center;
      margin: 0 auto 10px auto;
      color: #FFFFFF;
      font-size: 26px;
    }
    
    .user-name {
      font-weight: 700;
      color: #FFFFFF;
      font-size: 14px;
    }
    
    .user-role {
      font-size: 11px;
      color: #E5E7EB;
      text-transform: uppercase;
      letter-spacing: 0.5px;
    }

    
    /* ===== Tab Page Title ===== */
    .tab-page-title { 
      font-weight: 700; 
      color: #1E3A8A; 
      margin-bottom: 20px; 
      display: flex; 
      align-items: center; 
      gap: 12px; 
      font-size: 28px;        
      background-color: #FFFFFF;  
      padding: 16px 24px;       
      border-radius: 14px;
      box-shadow: 0 4px 12px rgba(0,0,0,0.05);
    }
    
    /* ===== CONTENT AREA ===== */
    .content-wrapper,
    .right-side {
      background-color: #F8FAFC !important;
    }
    
    /* ===== BOX / CARDS ===== */
    .box {
      border-radius: 14px !important;
      border: 1px solid #E5E7EB;
      box-shadow: 0 8px 24px rgba(0,0,0,0.05);
    }
    
    .small-box {
      border-radius: 14px !important;
      box-shadow: 0 8px 24px rgba(0,0,0,0.06);
    }
    .small-box .icon {
      opacity: 0.25;
    }
    
    /* ===== BUTTONS ===== */
    .btn {
      border-radius: 10px;
      font-weight: 600;
      padding: 6px 14px;
      margin-bottom: 5px;
    }
    .btn-primary {
      background-color: #1D4ED8;
      border-color: #1D4ED8;
    }
    .btn-success {
      background-color: #16A34A;
      border-color: #16A34A;
    }
    .btn-danger {
      background-color: #DC2626;
      border-color: #DC2626;
    }
    
    /* ===== BOX HEADER ===== */
    .box-header {
      border-radius: 14px 14px 0 0; 
      background-color: #1E40AF !important;
      border-bottom: none !important;
      text-align: left !important;
      padding: 14px 18px;
    }
    
    .box-header .box-title {
      color: #FFFFFF !important;
      font-weight: 700;
      font-size: 14px;
      letter-spacing: 0.5px;
    }

    /* ===== KPI CARDS ===== */
    .kpi-card {
      background: #FFFFFF;
      border-radius: 14px;
      padding: 18px 20px;
      box-shadow: 0 8px 24px rgba(0,0,0,0.05);
      display: flex;
      justify-content: space-between;
      align-items: center;
      height: 110px;
      margin-bottom: 5px;
    }
    
    .kpi-title {
      font-size: 12px;
      font-weight: 700;
      color: #1E3A8A;
      text-transform: uppercase;
    }
    
    .kpi-value {
      font-size: 30px;
      font-weight: 800;
      color: #1E3A8A;
    }
    
    .kpi-icon {
      width: 44px;
      height: 44px;
      border-radius: 50%;
      background: #EEF2FF;
      display: flex;
      align-items: center;
      justify-content: center;
      color: #1E3A8A;
      font-size: 20px;
    }
    
    .kpi-row {
      margin-bottom: 24px;
    }
    
    /* ===== LEADERBOARD  ===== */
    .leaderboard-card {
      background: #FFFFFF;
      border-radius: 14px;
      padding: 16px 18px;
      border: 1px solid #E5E7EB;
      box-shadow: 0 8px 24px rgba(0,0,0,0.05);
      margin-bottom: 10px;
    }
    
    .leaderboard-title {
      font-weight: 800;
      font-size: 13px;
      letter-spacing: 1px;
      color: #1E3A8A;
      margin-bottom: 14px;
      text-transform: uppercase;
    }
    
    /* Row */
    .leaderboard-row {
      display: flex;
      align-items: center;
      justify-content: space-between;
      padding: 8px 12px;
      border-radius: 10px;
      margin-bottom: 8px;
      background: #F8FAFC;
    }
    
    /* Stars */
    .gold-star { color: #FFD700; }  /* filled yellow */
    .blue-star { color: #D1D5DB; }  /* empty blue */
    
    /* Medals */
    .gold-medal { color: #FFD700; }
    .silver-medal { color: #C0C0C0; }
    .bronze-medal { color: #CD7F32; }
    
    /* Leaderboard row */
    .leaderboard-row {
      display: flex;
      align-items: center;
      justify-content: space-between;
      padding: 8px 12px;
      border-radius: 10px;
      margin-bottom: 8px;
      background: #F8FAFC;
    }
    
    .lb-name {
      font-weight: 700;
      margin-left: 8px;
    }
    
    .lb-count {
      background: #E0E7FF;
      color: #1E3A8A;
      font-weight: 800;
      padding: 4px 10px;
      border-radius: 999px;
      min-width: 42px;
      text-align: center;
    }
    
    /* ===== Medal / Rank alignment fix ===== */
    .lb-rank {
      width: 28px;              
      display: flex;
      justify-content: center;
      align-items: center;
      font-weight: 700;
      font-size: 16px;
      margin-right: 6px;
    }
    
    .lb-avatar {
      width: 36px;
      height: 36px;
      border-radius: 50%;
      background: #E0E7FF;
      display: flex;
      align-items: center;
      justify-content: center;
      color: #1E3A8A;
      font-size: 16px;
      margin-right: 10px;
      flex-shrink: 0;
      margin-left: 10px;
    }
    
    /* Left side group */
    .lb-left {
      display: flex;
      align-items: center;
    }
    
    /* ===== Name + stars layout ===== */
    .lb-info {
      display: flex;
      align-items: center;
      gap: 8px;
    }
    
    /* Stars inline on desktop */
    .lb-stars {
      margin-top: 0;
    }
    
    /* ===== Mobile behavior ===== */
    @media (max-width: 768px) {
      .lb-info {
        flex-direction: column;     /* stack */
        align-items: flex-start;
        gap: 2px;
      }
    
      .lb-stars {
        margin-top: 2px;            /* small spacing under name */
      }
    }

    .stat-item {
      display: flex;
      justify-content: space-between;
      align-items: center;
      padding: 10px 14px;
      margin-bottom: 8px;
      background: #F8FAFC;
      border-radius: 12px;
      font-size: 13px;
      color: #1F2937;
    }
    
    .stat-value {
      background: #E0E7FF;
      color: #1E3A8A;
      font-weight: 600;
      padding: 4px 10px;
      border-radius: 999px;
      font-size: 12px;
    }

    /* ===== DATATABLE ===== */
    table.dataTable {
      border-collapse: separate !important;
      border-spacing: 0 8px;
    }
    
    table.dataTable thead th {
      background: #EEF2FF;
      color: #1E3A8A;
      font-weight: 700;
      font-size: 13px;
      border: none;
    }
    
    table.dataTable tbody tr {
      background: #FFFFFF;
      box-shadow: 0 4px 10px rgba(0,0,0,0.04);
    }
    
    table.dataTable tbody td {
      border: none;
      font-size: 13px;
      color: #1F2937;
    }
    
    table.dataTable tbody tr:hover {
      background: #F1F5F9;
    }
    
    .dataTables_wrapper .dataTables_paginate .paginate_button {
      border-radius: 8px !important;
      border: none !important;
      background: #E0E7FF !important;
      color: #FFFFFF !important;
      font-weight: 600;
    }
    
    /* ===== FORMS ===== */
    select,
    textarea {
      border-radius: 10px !important;
      border: 1px solid #E5E7EB !important;
      padding: 8px 12px;
      font-size: 13px;
      color: #1F2937;
    }
    
    input:focus,
    select:focus,
    textarea:focus {
      border-color: #1E40AF !important;
      box-shadow: 0 0 0 2px rgba(30,64,175,0.15);
      outline: none !important;
    }
    
    label {
      font-size: 12px;
      font-weight: 700;
      color: #1E3A8A;
      text-transform: uppercase;
      letter-spacing: 0.5px;
    }

    /* ===== TEXT / ACTION BUTTONS ===== */
    .btn {
      border-radius: 10px;
      font-weight: 700;
      font-size: 13px;
      padding: 6px 14px;
      color: #FFFFFF !important;
      transition: background-color 0.2s ease;
    }
    
    .btn:hover {
      filter: brightness(90%);
      color: #FFFFFF !important;
    }
    
    .status-pill {
      padding: 4px 12px;
      border-radius: 999px;
      font-size: 12px;
      font-weight: 600;
      display: inline-block;
    }
      
      .cars-container {
        display: grid;
        grid-template-columns: repeat(auto-fill, minmax(200px, 1fr));
        gap: 15px;
      }
      .car-card {
        border:1px solid #ddd; 
        border-radius:8px; 
        padding:10px; 
        text-align:center; 
        cursor:pointer;
        transition: all 0.2s;
        background-color: #fff;
      }
      .car-card:hover {
        box-shadow: 0 4px 8px rgba(0,0,0,0.2);
      }
      .car-card img {
        width:100%; 
        height:120px; 
        object-fit:cover; 
        border-radius:6px;
      }
      
      .car-card.selected {
        border: 2px solid #007bff;
        box-shadow: 0 4px 12px rgba(0, 123, 255, 0.4);
      }
      
      .car-card h4 {
        color: #FACC15;        
        font-weight: 800;
        font-size: 16px;
        margin-top: 8px;
        margin-bottom: 6px;
      }
      
      .car-card p {
        color: #1E3A8A;        /* darker blue */
        font-size: 13px;
        margin: 2px 0;
        font-weight: 600;
      }
      
      /* Hover effect */
      .car-card:hover h4 {
        color: #EAB308;        /* darker yellow on hover */
      }
      
        
        /* ===== Cars Pagination Buttons ===== */
      #next_page,
      #prev_page {
        background-color: #1D4ED8 !important;  /* blue */
        color: #FFFFFF !important;
        border: none !important;
        font-weight: 700;
      }
      
      #next_page:hover,
      #prev_page:hover {
        background-color: #1E40AF !important;  
        color: #FFFFFF !important;
      }

      
      .available { background:#DCFCE7; color:#166534; }
      .rented { background:#FEE2E2; color:#991B1B; }
      .maintenance { background:#FEF3C7; color:#92400E; }
      
      .ongoing { background:#DBEAFE; color:#1D4ED8; }
      .reserved { background:#FDE68A; color:#92400E; }   
      .ended { background:#E5E7EB; color:#374151; }
    "))
          ),
          tabItems(
            tabItem(tabName = "dashboard",
                    uiOutput("tab_title_dashboard"),
                    fluidRow(
                      class = "kpi-row",
                      uiOutput("totalCarsBox"),
                      uiOutput("availableCarsBox"),
                      uiOutput("occupancyBox"),
                      uiOutput("monthRevenueBox")
                    ),
                    fluidRow(
                      column(6,
                             div(class = "leaderboard-card",
                                 div(class = "leaderboard-title", "Rentals Over Time"),
                                 plotlyOutput("rentalsTimePlot", height = "320px")
                             )
                      ),
                      column(6,
                             div(class = "leaderboard-card",
                                 div(class = "leaderboard-title", "Revenue Over Time"),
                                 plotlyOutput("revenueTimePlot", height = "320px")
                             )
                      )
                    ),
                    fluidRow(
                      box(width = 6,
                          div(class = "leaderboard-card",
                              div(class = "leaderboard-title", "Top Rented Cars"),
                              uiOutput("carsLeaderboard")
                          )
                      ),
                      box(width = 6,
                          div(class = "leaderboard-card",
                              div(class = "leaderboard-title", "Top Customers"),
                              uiOutput("customersLeaderboard")
                          )
                      )
                    )
            ),
            tabItem(tabName = "cars",
                    uiOutput("tab_title_cars"),
                    fluidRow(
                      box(width = 4, title = "Add / Update Car", solidHeader = TRUE, status = "primary",
                          textInput("car_brand", HTML("Brand <span style='color:red;'>*</span>"), placeholder = "Enter car brand"),
                          textInput("car_model", HTML("Model <span style='color:red;'>*</span>"), placeholder = "Enter car model"),
                          numericInput("car_year", HTML("Year <span style='color:red;'>*</span>"), value = year(Sys.Date()), min = 1900, max = 2100),
                          textInput("car_type", HTML("Type <span style='color:red;'>*</span>"), placeholder = "Enter car type"),
                          numericInput("car_price", HTML("Price Per Day <span style='color:red;'>*</span>"), value = NA, min = 0, step = 100),
                          uiOutput("car_status_ui"),
                          uiOutput("car_image_ui"),
                          
                          actionButton("add_car_btn", " Add Car", icon = icon("plus"), class = "btn-success"),
                          actionButton("update_car_btn", " Update Car", icon = icon("edit"), class = "btn-primary"),
                          actionButton("delete_car_btn", " Delete Car", icon = icon("trash"), class = "btn-danger"),
                          br(), br(),
                          helpText("Select a row on the table to populate fields for update/delete.")
                      ),
                      box(width = 8, title = "Cars", solidHeader = TRUE, status = "primary",
                          fluidRow(
                            column(6,
                                   textInput("car_search", NULL, placeholder = "Search brand, model, type...")
                            ),
                            column(6,
                                   actionButton("search_car_btn", "Search", icon = icon("search"),
                                                class = "btn-primary", width = "100%")
                            )
                          ),
                          br(),
                          
                          uiOutput("cars_cards"),
                          
                          br(),
                          fluidRow(
                            column(4,
                                   actionButton("prev_page", "Previous", icon = icon("chevron-left"),
                                                class = "btn-default", width = "100%")
                            ),
                            column(4,
                                   div(style = "text-align:center; padding-top:8px;",
                                       textOutput("page_info"))
                            ),
                            column(4,
                                   actionButton("next_page", "Next", icon = icon("chevron-right"),
                                                class = "btn-default", width = "100%")
                            )
                          ),
                          # JS for selecting car card
                          tags$script(HTML("
                         $(document).on('click', '.car-card', function() {
                           // Remove selection from other cards
                           $('.car-card').removeClass('selected');
                           // Add selection to clicked card
                           $(this).addClass('selected');
                           // Send car_id to Shiny
                           var carId = $(this).data('car-id');
                           Shiny.setInputValue('selected_car_id', carId, {priority: 'event'});
                         });
                       "))
                      )
                    )
            ),
            tabItem(tabName = "booking",
                    uiOutput("tab_title_booking"),
                    fluidRow(
                      box(width = 5, title = "Create / Manage Booking", solidHeader = TRUE, status = "primary",
                          fluidRow(
                            column(6,
                                   textInput("cust_name", HTML("Customer Name <span style='color:red;'>*</span>"), placeholder = "Enter customer full name", width = "100%"),
                                   textInput("cust_email", HTML("Email <span style='color:red;'>*</span>"), placeholder = "Enter email", width = "100%"),
                                   dateInput("end_date", HTML("End Date <span style='color:red;'>*</span>"), value = Sys.Date() + 1, width = "100%"),
                                   numericInput("car_price_day", HTML("Price per day <span style='color:red;'>*</span>"), value = 0, min = 0, step = 1, width = "100%"),
                                   uiOutput("booking_status_ui")
                            ),
                            column(6,
                                   textInput("cust_contact", HTML("Contact <span style='color:red;'>*</span>"), placeholder = "Enter contact number", width = "100%"),
                                   dateInput("start_date", HTML("Start Date <span style='color:red;'>*</span>"), value = Sys.Date(), width = "100%"),
                                   uiOutput("car_select_ui"),  # the dropdown for selecting available car
                                   numericInput("total_amount", HTML("Total Amount <span style='color:red;'>*</span>"), value = 0, min = 0, step = 1, width = "100%")
                            )
                          ),
                          actionButton("book_btn", "Confirm Booking", icon = icon("check"), class = "btn-success"),
                          actionButton("update_booking_btn", "Update Booking", icon = icon("edit"), class = "btn-primary"),
                          actionButton("delete_booking_btn", "Delete Booking", icon = icon("trash"), class = "btn-danger"),
                          br(), br(),
                          helpText("Select a row on the table to populate fields for update/delete.")
                      ),
                      box(width = 7, title = "Rental History", solidHeader = TRUE, status = "primary",
                          DTOutput("booking_table"))
                    )
            ),
            tabItem(tabName = "customers",
                    uiOutput("tab_title_customers"),
                    fluidRow(
                      box(title = "Customers", width = 12, solidHeader = TRUE, status = "primary",
                          DTOutput("customer_table"))
                    )
            )
          )
        )
      )
    }
  })
  
  poll_interval_ms <- 3000
  
  # ---------------- Body Page Titles ----------------
  output$tab_title_dashboard <- renderUI({
    tags$div(class = "tab-page-title", icon("tachometer-alt"), "DASHBOARD")
  })
  output$tab_title_cars <- renderUI({
    tags$div(class = "tab-page-title", icon("car"), "CAR MANAGEMENT")
  })
  output$tab_title_booking <- renderUI({
    tags$div(class = "tab-page-title", icon("calendar-check"), "BOOKING MANAGEMENT")
  })
  output$tab_title_customers <- renderUI({
    tags$div(class = "tab-page-title", icon("users"), "CUSTOMER LIST")
  })
  
  
  cars_df <- reactivePoll(
    poll_interval_ms, session,
    checkFunc = function() {
      r <- dbGetQuery(con, "SELECT MAX(updated_at) AS last FROM Cars")
      as.character(r$last)
    },
    valueFunc = function() {
      safe_query("SELECT * FROM Cars ORDER BY car_id ASC")
    }
  )
  
  bookings_df <- reactivePoll(
    poll_interval_ms, session,
    checkFunc = function() {
      r <- dbGetQuery(con, "SELECT MAX(updated_at) AS last FROM Bookings")
      as.character(r$last)
    },
    valueFunc = function() {
      safe_query("
      SELECT booking_id, customer_id, car_id, start_date, end_date, total_amount, status
      FROM Bookings
      ORDER BY booking_id DESC
    ")
    }
  )
  
  customers_df <- reactivePoll(
    poll_interval_ms, session,
    checkFunc = function() {
      r <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM Customers")
      r$n
    },
    valueFunc = function() {
      safe_query("SELECT * FROM Customers ORDER BY customer_id ASC")
    }
  )
  
  
  editing_booking <- reactiveVal(FALSE)
  selected_booking_car <- reactiveVal(NULL)
  cars_data <- reactiveVal(safe_query("SELECT * FROM Cars ORDER BY car_id ASC"))
  bookings_data <- reactiveVal(safe_query("SELECT booking_id, customer_id, car_id, start_date, end_date, total_amount, status FROM Bookings ORDER BY booking_id DESC"))
  
  # ---------------- Dashboard KPIs ----------------
  output$totalCarsBox <- renderUI({
    div(
      class = "col-md-3",
      div(class = "kpi-card",
          div(
            div(class = "kpi-title", "Total Cars"),
            div(class = "kpi-value", nrow(cars_df()))
          ),
          div(class = "kpi-icon", icon("car"))
      )
    )
  })
  
  output$availableCarsBox <- renderUI({
    avail <- cars_df() %>% filter(status == "available") %>% nrow()
    
    div(
      class = "col-md-3",
      div(class = "kpi-card",
          div(
            div(class = "kpi-title", "Available Cars"),
            div(class = "kpi-value", avail)
          ),
          div(class = "kpi-icon", icon("check"))
      )
    )
  })
  
  output$occupancyBox <- renderUI({
    total <- nrow(cars_df())
    rented <- cars_df() %>% filter(status == "rented") %>% nrow()
    pct <- ifelse(total == 0, 0, round(100 * rented / total, 1))
    
    div(
      class = "col-md-3",
      div(class = "kpi-card",
          div(
            div(class = "kpi-title", "Occupancy Rate"),
            div(class = "kpi-value", paste0(pct, "%"))
          ),
          div(class = "kpi-icon", icon("chart-pie"))
      )
    )
  })
  
  output$monthRevenueBox <- renderUI({
    df <- bookings_df()
    
    rev <- if (nrow(df) == 0) {
      0
    } else {
      df$start_date <- as.Date(df$start_date)
      mstart <- floor_date(Sys.Date(), "month")
      mend <- ceiling_date(Sys.Date(), "month") - days(1)
      
      sum <- df %>%
        filter(start_date >= mstart & start_date <= mend) %>%
        summarize(sum = sum(total_amount, na.rm = TRUE)) %>%
        pull(sum)
      
      ifelse(is.na(sum), 0, round(sum, 2))
    }
    
    div(
      class = "col-md-3",
      div(class = "kpi-card",
          div(
            div(class = "kpi-title", "Revenue This Month"),
            div(class = "kpi-value", paste0("₱", format(rev, big.mark = ",")))
          ),
          div(class = "kpi-icon", icon("peso-sign"))
      )
    )
  })
  
  
  # ---------------- Dashboard Charts ----------------
  output$rentalsTimePlot <- renderPlotly({
    df <- bookings_df()
    if (nrow(df) == 0) return(NULL)
    
    df$start_date <- as.Date(df$start_date)
    dfm <- df %>%
      mutate(month = floor_date(start_date, "month")) %>%
      group_by(month) %>%
      summarize(rentals = n(), .groups = "drop")
    
    plot_ly(
      dfm,
      x = ~month,
      y = ~rentals,
      type = "bar",
      marker = list(color = "#FBBF24"),
      text = NULL,                     
      textposition = "none",            
      hovertemplate = "Rentals: %{y}<extra></extra>"
    ) %>%
      layout(
        plot_bgcolor  = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        xaxis = list(
          title = "Month",
          gridcolor = "#E5E7EB",
          tickformat = "%b",   # abbreviated month
          dtick = "M1"         # ensures 1 tick per month
        ),
        yaxis = list(title = "Rentals", gridcolor = "#E5E7EB"),
        font  = list(color = "#1E3A8A" )
      )
    
  })
  
  output$revenueTimePlot <- renderPlotly({
    df <- bookings_df()
    if (nrow(df) == 0) return(NULL)
    
    df$start_date <- as.Date(df$start_date)
    drm <- df %>%
      mutate(month = floor_date(start_date, "month")) %>%
      group_by(month) %>%
      summarize(revenue = sum(total_amount, na.rm = TRUE), .groups = "drop")
    
    plot_ly(
      drm,
      x = ~month,
      y = ~revenue,
      type = "scatter",
      mode = "lines+markers",
      line = list(
        color = "#FACC15",   # yellow line
        width = 3
      ),
      fill = "tozeroy",      # 🔥 fills under the line
      fillcolor = "rgba(250, 204, 21, 0.35)",  # yellow w/ transparency
      text = ~format(month, "%B"),
      hovertemplate = "%{text}<br>Revenue: ₱%{y:,.2f}<extra></extra>"
    ) %>%
      layout(
        plot_bgcolor  = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        xaxis = list(
          title = "Month",
          gridcolor = "#E5E7EB",
          tickformat = "%b",
          dtick = "M1"  # 1 tick per month
        ),
        yaxis = list(title = "Revenue", gridcolor = "#E5E7EB"),
        font  = list(color = "#1E3A8A")
      )
    
  })
  
  
  # ---- Cars Leaderboard ----
  output$carsLeaderboard <- renderUI({
    dfb <- bookings_df()
    cars <- cars_df()
    if (nrow(dfb) == 0) return(NULL)
    
    top_cars <- dfb %>%
      group_by(car_id) %>%
      summarise(
        rent_count = n(),
        total_amount = sum(total_amount, na.rm = TRUE)
      ) %>%
      left_join(cars, by = "car_id") %>%
      mutate(car_name = paste(brand, model)) %>%
      arrange(desc(rent_count), desc(total_amount)) %>%  # <-- break ties by total_amount
      slice_head(n = 5)
    
    max_stars <- 5
    
    lapply(seq_len(nrow(top_cars)), function(i) {
      medal_icon <- switch(i,
                           "1" = "🥇",
                           "2" = "🥈",
                           "3" = "🥉",
                           tags$span(i),
                           tags$span(i))
      
      yellow_stars <- max_stars - (i - 1)
      blue_stars <- max_stars - yellow_stars
      
      stars_html <- paste0(
        paste(rep(as.character(icon("star", class = "fas gold-star")), yellow_stars), collapse = ""),
        paste(rep(as.character(icon("star", class = "blue-star")), blue_stars), collapse = "")
      )
      
      div(class = "leaderboard-row",
          div(class = "lb-left",
              div(class = "lb-rank", medal_icon),
              div(class = "lb-avatar", icon("car")),
              div(class = "lb-info",
                  span(class = "lb-name", top_cars$car_name[i]),
                  div(class = "lb-stars", HTML(stars_html))
              )
          ),
          span(class = "lb-count", top_cars$rent_count[i])
      )
      
    })
  })
  
  # ---- Customers Leaderboard ----
  output$customersLeaderboard <- renderUI({
    dfb <- bookings_df()
    cust <- customers_df()
    if (nrow(dfb) == 0) return(NULL)
    
    top_customers <- dfb %>%
      group_by(customer_id) %>%
      summarise(
        booking_count = n(),
        total_amount = sum(total_amount, na.rm = TRUE)
      ) %>%
      left_join(cust, by = "customer_id") %>%
      arrange(desc(booking_count), desc(total_amount)) %>%  # <-- break ties by total_amount
      slice_head(n = 5)
    
    max_stars <- 5
    
    lapply(seq_len(nrow(top_customers)), function(i) {
      medal_icon <- switch(i,
                           "1" = "🥇",
                           "2" = "🥈",
                           "3" = "🥉",
                           tags$span(i),
                           tags$span(i))
      
      yellow_stars <- max_stars - (i - 1)
      blue_stars <- max_stars - yellow_stars
      
      stars_html <- paste0(
        paste(rep(as.character(icon("star", class = "fas gold-star")), yellow_stars), collapse = ""),
        paste(rep(as.character(icon("star", class = "blue-star")), blue_stars), collapse = "")
      )
      
      div(class = "leaderboard-row",
          div(class = "lb-left",
              div(class = "lb-rank", medal_icon),
              div(class = "lb-avatar", icon("user")),
              div(class = "lb-info",
                  span(class = "lb-name", top_customers$name[i]),
                  div(class = "lb-stars", HTML(stars_html))
              )
          ),
          span(class = "lb-count", top_customers$booking_count[i])
      )
      
    })
  })
  
  
  # ---------------- Cars Cards ----------------
  filtered_cars <- reactive({
    df <- cars_data()
    
    if (is.null(input$car_search) || input$car_search == "") {
      return(df)
    }
    
    key <- tolower(input$car_search)
    
    df %>%
      filter(
        grepl(key, tolower(brand)) |
          grepl(key, tolower(model)) |
          grepl(key, tolower(type)) |
          grepl(key, as.character(year))
      )
  })
  
  observeEvent(input$search_car_btn, {
    current_page(1)
  })
  
  paged_cars <- reactive({
    df <- filtered_cars()
    
    start <- (current_page() - 1) * cars_per_page + 1
    end   <- min(start + cars_per_page - 1, nrow(df))
    
    if (nrow(df) == 0) return(df)
    df[start:end, ]
  })
  
  observeEvent(input$next_page, {
    max_page <- ceiling(nrow(filtered_cars()) / cars_per_page)
    if (current_page() < max_page) {
      current_page(current_page() + 1)
    }
  })
  
  observeEvent(input$prev_page, {
    if (current_page() > 1) {
      current_page(current_page() - 1)
    }
  })
  
  observeEvent(input$next_page, {
    max_page <- ceiling(nrow(filtered_cars()) / cars_per_page)
    if (current_page() < max_page) {
      current_page(current_page() + 1)
    }
  })
  
  observeEvent(input$prev_page, {
    if (current_page() > 1) {
      current_page(current_page() - 1)
    }
  })
  
  output$page_info <- renderText({
    total <- nrow(filtered_cars())
    if (total == 0) return("No results")
    
    max_page <- ceiling(total / cars_per_page)
    paste("Page", current_page(), "of", max_page)
  })
  
  
  status_pill <- function(x) {
    cls <- tolower(x)
    label <- tools::toTitleCase(x)
    
    sprintf(
      '<span class="status-pill %s">%s</span>',
      cls, label
    )
  }
  
  # Store cars data
  cars_data <- reactiveVal(safe_query("SELECT * FROM Cars ORDER BY car_id ASC"))
  
  # Selected car row
  selected_car_row <- reactive({
    car_id <- input$selected_car_id
    if (is.null(car_id)) return(NULL)
    df <- cars_data()
    df[df$car_id == car_id, , drop = FALSE]
  })
  
  output$cars_cards <- renderUI({
    df <- paged_cars()
    if (nrow(df) == 0) {
      return(div(style = "text-align:center; color:#6B7280;", "No cars found"))
    }
    
    cards <- lapply(seq_len(nrow(df)), function(i) {
      car <- df[i, ]
      
      tags$div(
        class = "car-card",
        `data-car-id` = car$car_id,
        
        if (!is.na(car$image) && car$image != "")
          tags$img(src = car$image),
        
        tags$h4(paste(car$brand, car$model)),
        tags$p(paste("Year:", car$year)),
        tags$p(paste("Type:", car$type)),
        tags$p(paste("Price: ₱", format(car$price_per_day, big.mark = ",", nsmall = 2))),
        tags$span(
          class = paste("status-pill", tolower(car$status)),
          tools::toTitleCase(car$status)
        )
      )
    })
    
    tagList(
      tags$div(class = "cars-container", do.call(tagList, cards)),
      tags$script(HTML("
      $(document).on('click', '.car-card', function() {
        var carId = $(this).data('car-id');
        Shiny.setInputValue('selected_car_id', carId, {priority: 'event'});
      });
    "))
    )
  })
  
  
  # Fill form when car selected
  observeEvent(input$selected_car_id, {
    row <- selected_car_row()
    if (is.null(row) || nrow(row) == 0) return()
    
    updateTextInput(session, "car_brand", value = row$brand)
    updateTextInput(session, "car_model", value = row$model)
    updateNumericInput(session, "car_year", value = row$year)
    updateTextInput(session, "car_type", value = row$type)
    updateNumericInput(session, "car_price", value = row$price_per_day)
    updateSelectInput(session, "car_status", selected = row$status)
  }, ignoreNULL = TRUE)
  
  
  useShinyjs()
  
  # Reset car fields function
  reset_car_fields <- function(session) {
    updateTextInput(session, "car_brand", value = "")
    updateTextInput(session, "car_model", value = "")
    updateNumericInput(session, "car_year", value = year(Sys.Date()))
    updateTextInput(session, "car_type", value = "")
    updateNumericInput(session, "car_price", value = 0)
    updateSelectInput(session, "car_status", selected = "available")
    
    output$car_image_ui <- renderUI({
      fileInput(
        "car_image",
        "Car Image",
        accept = c("image/png", "image/jpeg")
      )
    })    
    session$sendCustomMessage("clear_selected_car", TRUE)
  }
  
  # JS handler to clear selected_car_id
  tags$script(HTML("
Shiny.addCustomMessageHandler('clear_selected_car', function(x) {
  Shiny.setInputValue('selected_car_id', null, {priority: 'event'});
});
"))
  
  observeEvent(input$add_car_btn, {
    if (input$car_brand == "" || input$car_model == "" || input$car_type == "" ||
        is.null(input$car_year) || is.null(input$car_price)) {
      showNotification("All fields are required!", type = "error")
      return()
    }
    
    # Handle image upload
    img_path <- NULL
    if (!is.null(input$car_image)) {
      img_file <- input$car_image
      ext <- tools::file_ext(img_file$name)
      img_name <- paste0("car_", as.integer(Sys.time()), ".", ext)
      dir.create("www/car_images", showWarnings = FALSE)
      img_path <- file.path("www/car_images", img_name)
      file.copy(img_file$datapath, img_path)
    }
    
    b <- DBI::dbQuoteString(con, input$car_brand)
    m <- DBI::dbQuoteString(con, input$car_model)
    t <- DBI::dbQuoteString(con, input$car_type)
    st <- DBI::dbQuoteString(con, input$car_status)
    img_db <- if (!is.null(input$car_image)) {
      DBI::dbQuoteString(con, paste0("car_images/", img_name))
    } else {
      "NULL"  
    }
    
    q <- paste0(
      "INSERT INTO Cars (brand, model, year, type, price_per_day, status, image) VALUES (",
      b, ",", m, ",", input$car_year, ",", t, ",", input$car_price, ",", st, ",", img_db, ")"
    )
    
    tryCatch({
      dbExecute(con, q)
      showNotification("Car added", type = "message")
      reset_car_fields(session)
      cars_data(safe_query("SELECT * FROM Cars ORDER BY car_id ASC"))
    }, error = function(e) {
      showNotification(paste("Add car error:", e$message), type = "error")
    })
  })
  
  observeEvent(input$update_car_btn, {
    row <- selected_car_row()
    if(is.null(row)) { 
      showNotification("Select a car to update", type = "warning")
      return() 
    }
    
    car_id <- as.integer(row$car_id)
    
    # Handle image upload
    img_db <- NULL
    if (!is.null(input$car_image)) {
      img_file <- input$car_image
      ext <- tools::file_ext(img_file$name)
      img_name <- paste0("car_", as.integer(Sys.time()), ".", ext)
      dir.create("www/car_images", showWarnings = FALSE)
      img_path <- file.path("www/car_images", img_name)
      file.copy(img_file$datapath, img_path)
      img_db <- DBI::dbQuoteString(con, paste0("car_images/", img_name))
    }
    
    b <- DBI::dbQuoteString(con, input$car_brand)
    m <- DBI::dbQuoteString(con, input$car_model)
    t <- DBI::dbQuoteString(con, input$car_type)
    st <- DBI::dbQuoteString(con, input$car_status)
    
    q <- paste0(
      "UPDATE Cars SET brand=", b,
      ", model=", m,
      ", year=", input$car_year,
      ", type=", t,
      ", price_per_day=", input$car_price,
      ", status=", st
    )
    if (!is.null(img_db)) q <- paste0(q, ", image=", img_db)
    q <- paste0(q, " WHERE car_id=", car_id)
    
    tryCatch({
      dbExecute(con, q)
      showNotification("Car updated", type = "message")
      reset_car_fields(session)
      cars_data(safe_query("SELECT * FROM Cars ORDER BY car_id ASC"))
    }, error = function(e) {
      showNotification(paste("Update car error:", e$message), type = "error")
    })
  })
  
  observeEvent(input$delete_car_btn, {
    row <- selected_car_row()
    if(is.null(row)) { showNotification("Select a car to delete", type = "warning"); return() }
    car_id <- as.integer(row$car_id)
    tryCatch({
      dbExecute(con, paste0("DELETE FROM Cars WHERE car_id=", car_id))
      showNotification("Car deleted", type = "message")
      reset_car_fields(session)
      cars_data(safe_query("SELECT * FROM Cars ORDER BY car_id ASC"))
    }, error = function(e) {
      showNotification(paste("Delete car error:", e$message), type = "error")
    })
  })
  
  
  
  # ---------------- Bookings form & table ----------------
  output$car_select_ui <- renderUI({
    req(input$start_date, input$end_date)
    
    start_d <- as.Date(input$start_date)
    end_d   <- as.Date(input$end_date)
    
    df_cars <- cars_df() %>% filter(status != "maintenance")
    
    booked_cars <- safe_query(paste0(
      "SELECT car_id FROM Bookings
     WHERE NOT (end_date < '", start_d, "' OR start_date > '", end_d, "')"
    ))
    
    available_cars <- df_cars %>%
      filter(!(car_id %in% booked_cars$car_id))
    
    if (editing_booking() && !is.null(selected_booking_car())) {
      selected_car <- df_cars %>%
        filter(car_id == selected_booking_car())
      
      available_cars <- bind_rows(available_cars, selected_car) %>%
        distinct(car_id, .keep_all = TRUE)
    }
    
    if (nrow(available_cars) == 0) {
      selectInput(
        "selected_car_for_booking",
        "Select Car",
        choices = c("No available cars" = ""),
        selected = ""
      )
    } else {
      choices <- setNames(
        available_cars$car_id,
        paste(available_cars$brand, available_cars$model)
      )
      div(
        tags$label(HTML("Select Car <span style='color:red;'>*</span>")),  
      selectInput(
        "selected_car_for_booking",
        label = NULL,  
        choices = choices,
        selected = as.character(selected_booking_car())
      )
      )
    }
  })
  
  output$car_image_ui <- renderUI({
    fileInput(
      "car_image",
      "Car Image",
      accept = c("image/png", "image/jpeg")
    )
  })
  
  output$booking_status_ui <- renderUI({
    div(
      tags$label(HTML("Booking Status <span style='color:red;'>*</span>")),
      selectInput(
        "booking_status", 
        label = NULL,  
        choices = c(
          "Select Booking Status" = "",
          "reserved",
          "ongoing",
          "ended"
        ),
        selected = ""
      )
    )
  })
  
  output$car_status_ui <- renderUI({
    div(
      tags$label(HTML("Car Status <span style='color:red;'>*</span>")),
      selectInput(
        "car_status", 
        label = NULL,  
        choices = c("Select Car Status" = "", "available", "rented", "maintenance"),
        selected = ""
      )
    )
  })
  
  observeEvent(input$selected_car_for_booking, {
    req(input$selected_car_for_booking)
    car_id <- as.integer(input$selected_car_for_booking)
    price <- safe_query(paste0("SELECT price_per_day FROM Cars WHERE car_id = ", car_id))$price_per_day
    if(length(price) == 0 || is.na(price)) price <- 0
    updateNumericInput(session, "car_price_day", value = price)
    if(!is.null(input$start_date) && !is.null(input$end_date)) {
      days <- as.numeric(as.Date(input$end_date) - as.Date(input$start_date)) + 1
      if(is.na(days) || days <= 0) {
        updateNumericInput(session, "total_amount", value = 0)
      } else {
        updateNumericInput(session, "total_amount", value = round(price * days, 2))
      }
    }
  })
  
  observeEvent(list(input$start_date, input$end_date, input$car_price_day), {
    price <- as.numeric(input$car_price_day)
    if(is.null(input$start_date) || is.null(input$end_date) || is.na(price)) {
      updateNumericInput(session, "total_amount", value = 0)
      return()
    }
    days <- as.numeric(as.Date(input$end_date) - as.Date(input$start_date)) + 1
    if(is.na(days) || days <= 0) {
      updateNumericInput(session, "total_amount", value = 0)
    } else {
      updateNumericInput(session, "total_amount", value = round(price * days, 2))
    }
  }, ignoreNULL = FALSE)
  
  
  selected_booking_row <- reactive({
    s <- input$booking_table_rows_selected
    if(length(s) == 0) return(NULL)
    
    b <- bookings_df()
    c <- cars_df() %>% select(car_id, brand, model)
    cust <- customers_df() %>% select(customer_id, name)
    
    b2 <- left_join(b, cust, by = "customer_id") %>% 
      left_join(c, by = "car_id") %>%
      mutate(Car = paste(brand, model, sep = " - ")) %>%
      select(booking_id, customer_id, car_id, Customer = name, Car, start_date, end_date, total_amount, status) %>%
      arrange(desc(as.Date(start_date)))
    
    b2[s, , drop = FALSE]
  })
  
  observe({
    row <- selected_booking_row()
    if (is.null(row)) return()
    
    editing_booking(TRUE)
    
    selected_booking_car(as.integer(row$car_id))
    
    updateDateInput(session, "start_date", value = as.Date(row$start_date))
    updateDateInput(session, "end_date", value = as.Date(row$end_date))
    
    # Customer info
    cust <- safe_query(paste0(
      "SELECT * FROM Customers WHERE customer_id = ", row$customer_id
    ))
    
    if (nrow(cust) > 0) {
      updateTextInput(session, "cust_name", value = cust$name)
      updateTextInput(session, "cust_contact", value = cust$contact)
      updateTextInput(session, "cust_email", value = cust$email)
    }
    
    updateSelectInput(session, "booking_status", selected = row$status)
    
    price <- safe_query(
      paste0("SELECT price_per_day FROM Cars WHERE car_id = ", row$car_id)
    )$price_per_day
    
    updateNumericInput(session, "car_price_day", value = price)
    updateNumericInput(session, "total_amount", value = row$total_amount)
  })
  
  # ---------------- Booking CRUD ----------------
  
  reset_booking_fields <- function(session) {
    updateTextInput(session, "cust_name", value = "")
    updateTextInput(session, "cust_contact", value = "")
    updateTextInput(session, "cust_email", value = "")
    updateSelectInput(session, "selected_car_for_booking", selected = character(0))
    updateNumericInput(session, "car_price_day", value = 0)
    updateDateInput(session, "start_date", value = Sys.Date())
    updateDateInput(session, "end_date", value = Sys.Date() + 1)
    updateNumericInput(session, "total_amount", value = 0)
    updateSelectInput(session, "booking_status", selected = "ongoing")
    
    editing_booking(FALSE)
    selected_booking_car(NULL)
    
    DT::dataTableProxy("booking_table") %>% DT::selectRows(NULL)
    
  }
  
  observeEvent(input$book_btn, {
    # -------------------- Validation --------------------
    if (input$cust_name == "" || input$cust_contact == "" || input$cust_email == "" ||
        is.null(input$selected_car_for_booking) || is.null(input$start_date) || is.null(input$end_date) ||
        input$booking_status == "") {
      showNotification("All fields are required!", type = "error")
      return()
    }
    
    start_date <- as.Date(input$start_date)
    end_date <- as.Date(input$end_date)
    
    if (is.na(start_date) || is.na(end_date) || end_date < start_date) {
      showNotification("End date must be on/after start date", type = "error")
      return()
    }
    
    # -------------------- Customer --------------------
    qcust <- paste0(
      "SELECT customer_id FROM Customers WHERE name = ",
      DBI::dbQuoteString(con, input$cust_name),
      " AND contact = ", DBI::dbQuoteString(con, input$cust_contact),
      " AND email = ", DBI::dbQuoteString(con, input$cust_email)
    )
    cust <- dbGetQuery(con, qcust)
    
    if (nrow(cust) == 0) {
      qins <- paste0(
        "INSERT INTO Customers (name, contact, email) VALUES (",
        DBI::dbQuoteString(con, input$cust_name), ",",
        DBI::dbQuoteString(con, input$cust_contact), ",",
        DBI::dbQuoteString(con, input$cust_email), ")"
      )
      tryCatch(dbExecute(con, qins), error = function(e) {
        showNotification(paste("Customer insert error:", e$message), type = "error")
        return()
      })
      # SQLite version of LAST_INSERT_ID()
      cust_id <- dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id
    } else {
      cust_id <- cust$customer_id[1]
    }
    
    if (is.na(cust_id)) {
      showNotification("Unable to get customer id", type = "error")
      return()
    }
    
    # -------------------- Booking --------------------
    car_id <- as.integer(input$selected_car_for_booking)
    total <- round(as.numeric(input$total_amount), 2)
    booking_status <- input$booking_status
    
    qbook <- paste0(
      "INSERT INTO Bookings (customer_id, car_id, start_date, end_date, total_amount, status) VALUES (",
      cust_id, ",",
      car_id, ",",
      DBI::dbQuoteString(con, as.character(start_date)), ",",
      DBI::dbQuoteString(con, as.character(end_date)), ",",
      total, ",",
      DBI::dbQuoteString(con, booking_status), ")"
    )
    
    tryCatch({
      dbExecute(con, qbook)
      showNotification(paste0("Booking created — Total: ₱", total), type = "message")
      
      update_car_status()
      reset_booking_fields(session)
      
      bookings_data(dbGetQuery(con, "SELECT booking_id, customer_id, car_id, start_date, end_date, total_amount, status FROM Bookings ORDER BY booking_id DESC"))
      cars_data(dbGetQuery(con, "SELECT * FROM Cars ORDER BY car_id ASC"))
      
    }, error = function(e) {
      showNotification(paste("Booking error:", e$message), type = "error")
    })
  })
  
  
  # -------------------- Car Status Update --------------------
  update_car_status <- function() {
    today <- Sys.Date()
    today_sql <- DBI::dbQuoteString(con, as.character(today))
    
    # Reset cars
    dbExecute(con, "UPDATE Cars SET status = 'available' WHERE status != 'maintenance'")
    
    # Set rented only if booking is active today
    dbExecute(con, paste0(
      "UPDATE Cars SET status = 'rented' 
     WHERE car_id IN (
       SELECT car_id FROM Bookings 
       WHERE status = 'ongoing' 
         AND ", today_sql, " BETWEEN start_date AND end_date
     )"
    ))
  }
  
  
  # -------------------- Update Booking --------------------
  observeEvent(input$update_booking_btn, {
    row <- selected_booking_row()
    if(is.null(row)) {
      showNotification("Select a booking to update", type = "warning")
      return()
    }
    
    # Update Customers table
    cust_id <- row$customer_id
    qcust <- paste0(
      "UPDATE Customers SET ",
      "name=", DBI::dbQuoteString(con, input$cust_name), ", ",
      "contact=", DBI::dbQuoteString(con, input$cust_contact), ", ",
      "email=", DBI::dbQuoteString(con, input$cust_email),
      " WHERE customer_id=", cust_id
    )
    
    # Update Bookings table
    qbook <- paste0(
      "UPDATE Bookings SET ",
      "car_id=", as.integer(input$selected_car_for_booking), ", ",
      "start_date=", DBI::dbQuoteString(con, as.character(input$start_date)), ", ",
      "end_date=", DBI::dbQuoteString(con, as.character(input$end_date)), ", ",
      "total_amount=", round(as.numeric(input$total_amount), 2), ", ",
      "status=", DBI::dbQuoteString(con, input$booking_status),
      " WHERE booking_id=", row$booking_id
    )
    
    tryCatch({
      dbExecute(con, qcust)  # Update customer info
      dbExecute(con, qbook)  # Update booking info
      
      # Update car status (SQLite-compatible)
      update_car_status <- function() {
        today <- Sys.Date()
        today_str <- as.character(today)
        
        # Reset cars
        dbExecute(con, "UPDATE Cars SET status = 'available' WHERE status != 'maintenance'")
        
        # Set rented if active booking today
        dbExecute(con, paste0(
          "UPDATE Cars SET status = 'rented' ",
          "WHERE car_id IN (SELECT car_id FROM Bookings ",
          "WHERE status = 'ongoing' AND '", today_str, "' BETWEEN start_date AND end_date)"
        ))
      }
      
      update_car_status()
      
      showNotification("Booking and customer info updated successfully", type = "message")
      reset_booking_fields(session)
      
      bookings_data(
        safe_query("SELECT booking_id, customer_id, car_id, start_date, end_date, total_amount, status FROM Bookings ORDER BY booking_id DESC")
      )
      cars_data(safe_query("SELECT * FROM Cars ORDER BY car_id ASC"))
      
    }, error = function(e) {
      showNotification(paste("Update error:", e$message), type = "error")
    })
  })
  
  # -------------------- Delete Booking --------------------
  observeEvent(input$delete_booking_btn, {
    row <- selected_booking_row()
    if(is.null(row)) { 
      showNotification("Select a booking to delete", type = "warning")
      return() 
    }
    
    tryCatch({
      # Delete the booking
      dbExecute(con, paste0("DELETE FROM Bookings WHERE booking_id=", row$booking_id))
      
      # Update car status
      update_car_status()
      
      # Check if the customer has any other bookings
      remaining <- dbGetQuery(con, paste0(
        "SELECT COUNT(*) AS n FROM Bookings WHERE customer_id=", row$customer_id
      ))$n
      
      # Delete customer if no more bookings
      if(remaining == 0) {
        dbExecute(con, paste0("DELETE FROM Customers WHERE customer_id=", row$customer_id))
        showNotification("Booking and customer deleted", type = "message")
      } else {
        showNotification("Booking deleted (customer has other bookings)", type = "message")
      }
      
      # Reset fields and refresh data
      reset_booking_fields(session)
      bookings_data(
        safe_query("SELECT booking_id, customer_id, car_id, start_date, end_date, total_amount, status FROM Bookings ORDER BY booking_id DESC")
      )
      cars_data(safe_query("SELECT * FROM Cars ORDER BY car_id ASC"))
      
    }, error = function(e) {
      showNotification(paste("Booking delete error:", e$message), type = "error")
    })
  })
  
  
  # Store bookings data
  bookings_data <- reactiveVal(
    safe_query("SELECT booking_id, customer_id, car_id, start_date, end_date, total_amount, status FROM Bookings ORDER BY booking_id DESC")
  )
  
  output$booking_table <- renderDT({
    b <- bookings_data()
    c <- cars_data() %>% select(car_id, brand, model)
    cust <- customers_df() %>% select(customer_id, name)
    
    if(nrow(b) > 0){
      b2 <- left_join(b, cust, by = "customer_id") %>%
        left_join(c, by = "car_id") %>%
        mutate(
          BookingID = sprintf("BOOK-%03d", booking_id),
          Car = paste(brand, model, sep = " - "),
          total_amount = paste0("₱", format(total_amount, big.mark = ",", nsmall = 2)),
          status = status_pill(status)
        ) %>%
        select(BookingID, name, Car, start_date, end_date, status, total_amount) %>%
        rename(
          Customer = name,
          Start    = start_date,
          End      = end_date,
          Total    = total_amount
        ) %>%
        arrange(desc(as.Date(Start)))
      
      datatable(
        b2,
        selection = "single",
        rownames = FALSE,
        colnames = c(
          "Booking ID", "Customer", "Car", "Start Date", "End Date", "Status", "Total Amount"
        ),
        extensions = "Responsive",   # <- enable responsive extension
        options = list(pageLength = 10, responsive = TRUE, scrollX = TRUE),
        escape = FALSE
      )
    } else {
      datatable(data.frame(), rownames = FALSE)
    }
  })
  
  # ---------------- Customers Table ----------------
  
  output$customer_table <- renderDT({
    df <- customers_df() %>%
      select(-updated_at) %>%
      mutate(
        customer_id = sprintf("CUST-%03d", customer_id)
      )
    
    datatable(
      df,
      rownames = FALSE,
      colnames = c(
        "Customer ID",
        "Name",
        "Contact",
        "Email"
      ),
      extensions = "Responsive",   # <- enable responsive extension
      options = list(pageLength = 10, responsive = TRUE, scrollX = TRUE)
    )
  })
  
  session$onSessionEnded(function() {
    tryCatch({ dbDisconnect(con) }, error = function(e) { })
  })
}

# -------------------------- RUN APP --------------------------
shinyApp(ui, server)
