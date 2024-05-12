# Code structure
./root_folder
	app/     <-- app folder contains implementation for this assignment
		models  <-- models folder contains implementation for db related functionalities
			connection.py
			paragraph_schema.py
			paragraph.py
		routes   <-- routes folder contains implementation for api endpoints
			endpoints.py
		utils      <-- utils folder contains implementation for utility funcs
			func.py
		main.py  <-- entry point of the service 
	test_app  <-- test_app folder contains test cases
		test_main.py
	db_schema.sql  <-- sql script to drop/create table in database
	docker-compose.yaml  <-- defines services which are database and backend-api
	Dockerfile  <-- defines docker image of backend-api
	requirements.txt  <-- defines required libraries
	start.sh <-- bash script to start everything up

# System design
- Storage layer:
	- Postgres. I decided to choose Postgres as It is a very mature database, in addition SQL table is also a good fit for storing data being used in this assignment
- Full text search via Postgres:
	- I decided to use full text search feature provided in Postgres to implement endpoints search and dictionary
	- Decision making reason:
		- Using only Postgres I don't need to add one more service/component into my design, It helps my design simplified in both implementation and deployment steps
		- Postgres full text search is well supported and seems to be a good solution when dealing with text search
- API:
	- Fast API framework, I chose it because of it's easy-to-use and yet very high performance backed by modern standards

# Prerequisite
- Python 3.9+
- Docker

# How to start service
`./start.sh`

# How to run test
`pip install requirements.txt`

`./start.sh && POSTGRES_URL=postgresql://postgres:12345@localhost:55432/postgres pytest`