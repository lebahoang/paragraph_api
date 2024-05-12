from traceback import print_tb
from sqlalchemy import Index
from sqlalchemy import Column, Computed, DateTime, Integer, String
from sqlalchemy.dialects.postgresql import TSVECTOR
from sqlalchemy.orm import Session
from sqlalchemy.sql import func
from app.models.paragraph_schema import SearchParagraphs, OP, Stat
from .connection import Base
from sqlalchemy import types

class TSVector(types.TypeDecorator):
    impl = TSVECTOR

class Paragraphs(Base):
    __tablename__ = "paragraphs"

    id = Column(Integer, primary_key=True, index=True)
    paragraph = Column(String)
    vector = Column(TSVector(), Computed("to_tsvector('simple', paragraph)", persisted=True))
    created_at = Column(DateTime(timezone=True), server_default=func.now())

    __table_args__ = (Index('ix_paragraphs_vector',
        vector, postgresql_using='gin'),)

def create_paragraph(db: Session, p: str) -> Paragraphs:
    p_model = Paragraphs(paragraph = p)
    db.add(p_model)
    db.commit()
    db.refresh(p_model)
    return p_model

def search_paragraphs(db: Session, search_req: SearchParagraphs) -> list[Paragraphs]:
    op = ''
    if search_req.op == OP.OR.value:
        op = ' | '
    else:
        op = ' & '
    op = op.join(search_req.words)
    rs = []
    select_query = """
        SELECT id, paragraph, created_at
        FROM paragraphs
        WHERE vector @@ to_tsquery(:op);
    """
    result_set = db.execute(select_query, {'op': op})
    for r in result_set:
        rs.append(Paragraphs(id = r[0], paragraph = r[1], created_at = r[2]))
    return rs
def dictionary(db: Session) -> list[str]:
    vectorTable = 'SELECT vector from paragraphs'
    query = """
        SELECT word, nentry
        FROM ts_stat('{}')
        ORDER BY nentry DESC
        LIMIT :limit;
    """.format(vectorTable)
    rs = []
    result_set = db.execute(query, {'limit': Stat.LIMIT.value})
    for r in result_set:
        rs.append(r[0])
    return rs


        
