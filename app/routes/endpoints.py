import traceback
from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy.orm import Session
from app.models.connection import get_db
from app.models.paragraph_schema import Paragraphs, SearchParagraphs, DictionaryResponse
import app.models.paragraph as m
import requests
import app.utils.func as func


router = APIRouter()
metaphorpsumURL = 'http://metaphorpsum.com'
n_paragraphs = 1
n_sentences = 50

dictionaryURL = 'https://api.dictionaryapi.dev/api/v2/entries/en'

@router.get('/get', response_model=Paragraphs)
def create_paragraph(db: Session = Depends(get_db)):
    try:
        page = requests.get('{}/paragraphs/{:d}/{:d}'.format(metaphorpsumURL, n_paragraphs, n_sentences))
        p_model = m.create_paragraph(db, page.text)
        return p_model
    except Exception:
        traceback.print_exc()
        raise HTTPException(status_code=500, detail='Internal error')

@router.post('/search', response_model=list[Paragraphs])
def search_paragraphs(search_req: SearchParagraphs, db: Session = Depends(get_db)):
    try:
        return m.search_paragraphs(db, search_req)
    except Exception:
        traceback.print_exc()
        raise HTTPException(status_code=500, detail='Internal error')

@router.get('/dictionary', response_model=list[DictionaryResponse])
async def get_dictionary(db: Session = Depends(get_db)):
    try:
        words = m.dictionary(db)
        reqs = []
        for w in words:
            reqs.append('{}/{}'.format(dictionaryURL, w))
        resp = await func.call(reqs)
        rs = []
        key = 'meanings'
        for w, data in zip(words, resp):
            if key in data:
                rs.append(DictionaryResponse(word = w, definitions = data[key]))
            else:
                rs.append(DictionaryResponse(word = w, definitions = []))
        return rs
    except Exception:
        traceback.print_exc()
        raise HTTPException(status_code=500, detail='Internal error')
