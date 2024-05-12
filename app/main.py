from fastapi import Depends, FastAPI
from .models.connection import Base, engine
from .routes import endpoints

Base.metadata.create_all(bind=engine)

app = FastAPI()
app.include_router(endpoints.router)