# aws-scheduler
Admission Workshop Interview Matrix Scheduler

Procedure to build AWS matrix schedule
Jeremie Gillet, 2017

1. Query the mySql database. Run checkDB as a test. Login info is in MysqlRequests.sh. 

2. Get Excel file from Academic Services Section: who interviews who.

3. Extract the useful information into a CSV:
  - Change the in-depth interviews from values 1 to 2 (orange and stripy green in Excel)
  
  - Sort the values by last name
  
  - Keep only columns: Last name, First name (or just ID) and all professor columns. Delete a few of the columns after that, they are formatted weirdly.
  
  - Keep only rows: with faculty names and with student info
  
  - Give a name to the CSV, put it inside Input_Output, change the relevant paths into shcedule.hs
  
4. run checkNames in scheduler.hs. There will be a list of namesfromt  he CSV that could not be matched with the names from the DB. Fix those. Common issues: tabs in names, non-ASCII characters, students not in CSV, students not in DB.

5. run main, send the output CSV file in Input_Output to Academic Services Section.
