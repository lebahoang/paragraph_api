from enum import Enum
from typing import Any
from wsgiref.validate import validator
from pydantic import BaseModel, validator
from datetime import datetime

class OP(Enum):
    OR = 'or'
    AND = 'and'
class Stat(Enum):
    LIMIT = 10

class SearchParagraphs(BaseModel):
    words: list[str]
    op: str

    @validator('op')
    def op_must_be_and_or(cls, v):
        if v != OP.AND.value and v != OP.OR.value:
            raise ValueError('op must be one of two operators: AND,OR')
        return v
    @validator('words')
    def list_words_must_be_non_empty(cls, v):
        if not v:
            raise ValueError('list words must be non-empty')
        return v


class Paragraphs(BaseModel):
    id: int
    paragraph: str
    created_at: datetime

    class Config:
        orm_mode = True

class DictionaryResponse(BaseModel):
    word: str
    definitions: list[dict[str, Any]]