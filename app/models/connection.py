import os
from sqlalchemy import create_engine
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import sessionmaker

SQLALCHEMY_DATABASE_URL = os.getenv('POSTGRES_URL', 'postgresql://postgres:12345@localhost:55432/postgres')

engine = create_engine(
    SQLALCHEMY_DATABASE_URL, connect_args={"sslmode": "disable"}
)
SessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=engine)

Base = declarative_base()

def get_db():
    db = SessionLocal()
    try:
        yield db
    finally:
        db.close()