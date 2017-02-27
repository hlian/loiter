--
-- PostgreSQL database dump
--

-- Dumped from database version 9.6.1
-- Dumped by pg_dump version 9.6.1

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: jobs; Type: TABLE; Schema: public; Owner: loiter
--

CREATE TABLE jobs (
    id integer NOT NULL,
    name jsonb NOT NULL,
    status integer NOT NULL,
    dt timestamp with time zone NOT NULL
);


ALTER TABLE jobs OWNER TO loiter;

--
-- Name: jobs_id_seq; Type: SEQUENCE; Schema: public; Owner: loiter
--

CREATE SEQUENCE jobs_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE jobs_id_seq OWNER TO loiter;

--
-- Name: jobs_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: loiter
--

ALTER SEQUENCE jobs_id_seq OWNED BY jobs.id;


--
-- Name: truth; Type: TABLE; Schema: public; Owner: loiter
--

CREATE TABLE truth (
    id integer NOT NULL,
    author text NOT NULL,
    body text NOT NULL,
    dt timestamp with time zone
);


ALTER TABLE truth OWNER TO loiter;

--
-- Name: truth_id_seq; Type: SEQUENCE; Schema: public; Owner: loiter
--

CREATE SEQUENCE truth_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE truth_id_seq OWNER TO loiter;

--
-- Name: truth_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: loiter
--

ALTER SEQUENCE truth_id_seq OWNED BY truth.id;


--
-- Name: jobs id; Type: DEFAULT; Schema: public; Owner: loiter
--

ALTER TABLE ONLY jobs ALTER COLUMN id SET DEFAULT nextval('jobs_id_seq'::regclass);


--
-- Name: truth id; Type: DEFAULT; Schema: public; Owner: loiter
--

ALTER TABLE ONLY truth ALTER COLUMN id SET DEFAULT nextval('truth_id_seq'::regclass);


--
-- Name: jobs jobs_pkey; Type: CONSTRAINT; Schema: public; Owner: loiter
--

ALTER TABLE ONLY jobs
    ADD CONSTRAINT jobs_pkey PRIMARY KEY (id);


--
-- Name: truth truth_pkey; Type: CONSTRAINT; Schema: public; Owner: loiter
--

ALTER TABLE ONLY truth
    ADD CONSTRAINT truth_pkey PRIMARY KEY (id);


--
-- Name: jobs_name_unique; Type: INDEX; Schema: public; Owner: loiter
--

CREATE UNIQUE INDEX jobs_name_unique ON jobs USING btree (name);


--
-- PostgreSQL database dump complete
--

