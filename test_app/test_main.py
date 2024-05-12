from enum import Enum
import os
import re
from fastapi.testclient import TestClient
from http import HTTPStatus
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker

from app.models.connection import Base, get_db
from app.models.paragraph import create_paragraph
from app.models.paragraph_schema import OP
from app.routes.endpoints import metaphorpsumURL, dictionaryURL
from app.main import app

import pytest
import requests_mock as req_mock

SQLALCHEMY_DATABASE_URL = os.getenv('POSTGRES_URL', 'postgresql://postgres:12345@localhost:55432/postgres') 

engine = create_engine(SQLALCHEMY_DATABASE_URL)
Base.metadata.create_all(bind=engine)
TestingSessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=engine)

class MockData(Enum):
    GET_MOCK_DATA = 'mock test'

def override_get_db():
    try:
        db = TestingSessionLocal()
        yield db
    finally:
        db.close()

app.dependency_overrides[get_db] = override_get_db

@pytest.fixture(scope='module')
def module_client():
    with TestClient(app) as c:
        yield c

@pytest.fixture
def client(module_client, requests_mock):
    test_app_base_url_prefix_regex = re.compile(fr"{re.escape(module_client.base_url)}(/.*)?")
    requests_mock.register_uri(req_mock.ANY, test_app_base_url_prefix_regex, real_http=True)
    metaphorpsum_base_url_regex = re.compile(fr"{re.escape(metaphorpsumURL)}(/.*)?")
    requests_mock.register_uri(req_mock.ANY, metaphorpsum_base_url_regex, text = MockData.GET_MOCK_DATA.value)
    dictionary_base_url_regex = re.compile(fr"{re.escape(dictionaryURL)}(/.*)?")
    requests_mock.register_uri(req_mock.ANY, dictionary_base_url_regex, text = '{}')
    return module_client

def test_endpoint_get(client):
    resp = client.get('/get')
    assert resp.status_code == HTTPStatus.OK
    data = resp.json()
    assert data['id'] > 0
    assert len(data['paragraph']) > 0
    assert data['paragraph'] == MockData.GET_MOCK_DATA.value

def test_endpoint_search_invalid(client):
    resp = client.post('/search', json = {
        'words': 'not_list',
        'op': 'or'
    })
    assert resp.status_code == HTTPStatus.UNPROCESSABLE_ENTITY

    resp = client.post('/search', json = {
        'words': 'not_list',
        'op': 'not_and_or'
    })
    assert resp.status_code == HTTPStatus.UNPROCESSABLE_ENTITY

    resp = client.post('/search', json = {
        'words': [],
        'op': 'or'
    })
    assert resp.status_code == HTTPStatus.UNPROCESSABLE_ENTITY

    resp = client.post('/search', json = {
        'words': ['abc', 'ewarea'],
        'op': ''
    })
    assert resp.status_code == HTTPStatus.UNPROCESSABLE_ENTITY

    resp = client.post('/search', json = {
        'words': ['abc', 'ewarea'],
        'op': 'xyz'
    })
    assert resp.status_code == HTTPStatus.UNPROCESSABLE_ENTITY


def test_endpoint_search(client):
    with TestingSessionLocal() as db:
        p1 = create_paragraph(db, 'abc')
        p2 = create_paragraph(db, 'xyz')
        random_str = 'random_string'
        for p in [p1, p2]:
            resp = client.post('/search', json = {
                'words': [p.paragraph, random_str],
                'op': OP.OR.value
            })
            assert resp.status_code == HTTPStatus.OK
            data = resp.json()
            assert len(data) > 0
            for record in data:
                assert str(record['paragraph']).find(p.paragraph) != -1
        for p in [p1, p2]:
            resp = client.post('/search', json = {
                'words': [p.paragraph, random_str],
                'op': OP.AND.value
            })
            assert resp.status_code == HTTPStatus.OK
            data = resp.json()
            assert len(data) == 0

def test_endpoint_dictionary(client):
    with TestingSessionLocal() as db:
        words = []
        n_paragraphs = 10
        n_docs = 100
        for i in range(1, n_paragraphs+1):
            word = 'paragraph' + str(i)
            arr = []
            for _ in range(n_docs-i):
                arr.append(word)
            p = ' '.join(arr)
            create_paragraph(db, p)
            words.append(word)
    
        top10 = 10
        resp = client.get('/dictionary')
        assert resp.status_code == HTTPStatus.OK
        data = resp.json()
        assert len(data) == top10
        for w, d in zip(words, data):
            assert 'word' in d
            assert d['word'] == w