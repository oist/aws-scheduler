# aws-scheduler
Admission Workshop Interview Matrix Scheduler

Procedure to build AWS matrix schedule
Jeremie Gillet, 2018

1. Query the mySql database. Run checkDB as a test. Login info is in Mysql.config.

2. Get Excel file from Academic Services Section: who interviews who.

3. Extract the useful information into a CSV:
  - Change the in-depth interviews from values 1 to 2 (orange and stripy green in Excel)

  - Keep only columns: ID and all professor columns. Delete a few of the columns after that, they are formatted weirdly.

  - Keep only rows: with faculty IDs and with student info

  - Give a name to the CSV, put it inside Input_Output, change the relevant paths into schedule.hs
  
4. run checkNames in scheduler.hs. Hopefully the IDs all check out.

5. run main, send the output CSV file in Input_Output to Academic Services Section.

———————

Possible improvements:
- The last students in the list only have late time slots, maybe rotating the order?
- Automatizing the "who interviews who" matrix creation
