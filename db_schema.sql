-- public.wagers definition

-- Drop table

DROP TABLE IF EXISTS
	public.paragraphs
CASCADE;

CREATE TABLE public.paragraphs (
	id bigserial NOT NULL,
	paragraph VARCHAR,
	vector tsvector GENERATED ALWAYS AS (to_tsvector('simple', paragraph)) STORED,
	created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP
);