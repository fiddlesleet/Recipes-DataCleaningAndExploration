import sqlite3 # library for the database used

'''
Return a connection object
'''
connection = sqlite3.connect("Cookies") # Cookies is the name of the DB we're connecting to

'''
Return cursor object, which runs queries & fetches results
    It's called a cursor because it's when the DB returns results, you use the cursor
    to scan through results, just like the cursor in a text editor
'''
cursor = connection.cursor()

# execute query using cursor
cursor.execute("select host_key from cookies limit 10")
# fetch all results from the query, by using the cursor
results = cursor.fetchall()

print results

#connection.commit() an insertion here, or connection.rcollback() if something went wrong

connection.close()