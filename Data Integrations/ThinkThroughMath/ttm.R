require(rvest)
require(httr)

url <- 'https://lms.thinkthroughmath.com'

uastring<-"Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"

uastring <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_3) AppleWebKit/537.75.14 (KHTML, like Gecko) Version/7.0.3 Safari/7046A194A"


ttm <- html_session(url, user_agent(uastring))


ttm2<-ttm %>%
  html_node("form") %>%
  html_form() %>%
  set_values('user[username]' = 'msalmonowicz@kippchicago.org',
             'user[password]' = 'ttm482') %>%
  submit_form(ttm, . )

# ttm2<-ttm %>%
# html_node("form") %>%
# html_form() %>%
# set_values('user[username]' = 'msalmonowicz@kippchicago.org',
# 'user[password]' = 'ttm482')
#
# submit_form(ttm, ttm2)


dashb <-jump_to(ttm, "https://lms.thinkthroughmath.com/dashboard")
dashb$url
dashb$html
dashb$response

rdom(dashb$ur)

overview <-jump_to(ttm, "https://lms.thinkthroughmath.com/dashboard/widgets/filter_children?is_default=false&parent_id=18286&parent_type=school&_=1462564397271")


data<-
  dashb %>%
  html_nodes("td")

r <- GET("https://lms.thinkthroughmath.com")
r <- POST("https://lms.thinkthroughmath.com/users/sign_in",
          body = list("user[username]" = "msalmonowicz@kippchicago.org",
                      "user[password]" = "ttm482",
                      "button" = ""),
          encode = "json")

r <- GET("https://lms.thinkthroughmath.com/dashboard")



# selenium

#startServer()
#remDrv <- remoteDriver()
pJS <- phantom()
remDrv <- remoteDriver(browserName = 'phantomjs')
remDrv$open()

remDrv$navigate("https://lms.thinkthroughmath.com/")

uid_elem <- remDrv$findElement(using = "xpath", '//*[(@id = "user_username")]')

uid_elem$sendKeysToElement(list("msalmonowicz@kippchicago.org"))

pwd_elem <- remDrv$findElement(using = "xpath", '//*[(@id = "user_password")]')

pwd_elem$sendKeysToElement(list("ttm482", key = "enter"))


remDrv$navigate("https://lms.thinkthroughmath.com/reports/overview_report#page=1&classroom=890372&id=890372&type=classroom&group=student&district=1649&district_name=KIPP+Chicago&school=18286&school_name=KIPP+Create+College+Prep&classroom_name=8th+Mathematics+-+3+-+4%28A%29&rows_per_page=1000")

overview_report<-remDrv$findElement(using = "class name", 'overview-report-student-table')

overview_report$getElementAttribute("table")

remDrv$quit()
remDrv$closeServer()
