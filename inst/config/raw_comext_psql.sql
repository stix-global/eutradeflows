--
-- Name: tradeflows; Type: SCHEMA; Schema: -; Owner: rdb
--

CREATE SCHEMA raw_comext;

ALTER SCHEMA raw_comext OWNER TO rdb;

--
-- Name: raw_comext.monthly; Type: TABLE; Schema: tradeflows; Owner: rdb
--

CREATE TABLE raw_comext.monthly (
    reportercode bigint,
    reporteriso character varying(2) DEFAULT NULL::character varying,
    partnercode bigint,
    partneriso character varying(2) DEFAULT NULL::character varying,
    tradetype character varying(1) DEFAULT NULL::character varying,
    productcode character varying(10) DEFAULT NULL::character varying,
    productsitc character varying(5) DEFAULT NULL::character varying,
    productcpa2002 character varying(5) DEFAULT NULL::character varying,
    productcpa2008 character varying(5) DEFAULT NULL::character varying,
    productcpa21 character varying(5) DEFAULT NULL::character varying,
    productbec character varying(5) DEFAULT NULL::character varying,
    productsection character varying(5) DEFAULT NULL::character varying,
    flowcode bigint,
    statregime bigint,
    unitcode character varying(1) DEFAULT NULL::character varying,
    period bigint,
    tradevalue double precision,
    weight double precision,
    quantity bigint
);


ALTER TABLE raw_comext.monthly OWNER TO rdb;

--
-- Name: raw_comext.monthly_template; Type: TABLE; Schema: tradeflows; Owner: rdb
--

CREATE TABLE raw_comext.monthly_template (
    reportercode bigint,
    reporteriso character varying(2) DEFAULT NULL::character varying,
    partnercode bigint,
    partneriso character varying(2) DEFAULT NULL::character varying,
    tradetype character varying(1) DEFAULT NULL::character varying,
    productcode character varying(10) DEFAULT NULL::character varying,
    productsitc character varying(5) DEFAULT NULL::character varying,
    productcpa2002 character varying(5) DEFAULT NULL::character varying,
    productcpa2008 character varying(5) DEFAULT NULL::character varying,
    productcpa21 character varying(5) DEFAULT NULL::character varying,
    productbec character varying(5) DEFAULT NULL::character varying,
    productsection character varying(5) DEFAULT NULL::character varying,
    flowcode bigint,
    statregime bigint,
    unitcode character varying(1) DEFAULT NULL::character varying,
    period bigint,
    tradevalue double precision,
    weight double precision,
    quantity bigint
);


ALTER TABLE raw_comext.monthly_template OWNER TO rdb;

--
-- Name: raw_comext.partner; Type: TABLE; Schema: tradeflows; Owner: rdb
--

CREATE TABLE raw_comext.partner (
    partnercode bigint,
    datestart date,
    dateend date,
    partner text,
    datestart2 date,
    dateend2 date
);


ALTER TABLE raw_comext.partner OWNER TO rdb;

--
-- Name: raw_comext.product; Type: TABLE; Schema: tradeflows; Owner: rdb
--

CREATE TABLE raw_comext.product (
    productcode character varying(10) DEFAULT NULL::character varying,
    datestart date,
    dateend date,
    productdescription text,
    datestart2 date,
    dateend2 date
);


ALTER TABLE raw_comext.product OWNER TO rdb;

--
-- Name: raw_comext.reporter; Type: TABLE; Schema: tradeflows; Owner: rdb
--

CREATE TABLE raw_comext.reporter (
    reportercode bigint,
    datestart date,
    dateend date,
    reporter text,
    datestart2 date,
    dateend2 date
);


ALTER TABLE raw_comext.reporter OWNER TO rdb;

--
-- Name: raw_comext.unit; Type: TABLE; Schema: tradeflows; Owner: rdb
--

CREATE TABLE raw_comext.unit (
    productcode character varying(10) DEFAULT NULL::character varying,
    unitcode character varying(1) DEFAULT NULL::character varying,
    datestart date,
    dateend date
);


ALTER TABLE raw_comext.unit OWNER TO rdb;

--
-- Name: raw_comext.unit_description; Type: TABLE; Schema: tradeflows; Owner: rdb
--

CREATE TABLE raw_comext.unit_description (
    unitcode character varying(1) DEFAULT NULL::character varying,
    datestart date,
    dateend date,
    unitdescription text,
    datestart2 date,
    dateend2 date
);


ALTER TABLE raw_comext.unit_description OWNER TO rdb;

--
-- Name: raw_comext.yearly; Type: TABLE; Schema: tradeflows; Owner: rdb
--

CREATE TABLE raw_comext.yearly (
    reportercode bigint,
    reporteriso character varying(2) DEFAULT NULL::character varying,
    partnercode bigint,
    partneriso character varying(2) DEFAULT NULL::character varying,
    tradetype character varying(1) DEFAULT NULL::character varying,
    productcode character varying(10) DEFAULT NULL::character varying,
    productsitc character varying(5) DEFAULT NULL::character varying,
    productcpa2002 character varying(5) DEFAULT NULL::character varying,
    productcpa2008 character varying(5) DEFAULT NULL::character varying,
    productcpa21 character varying(5) DEFAULT NULL::character varying,
    productbec character varying(5) DEFAULT NULL::character varying,
    productsection character varying(5) DEFAULT NULL::character varying,
    flowcode bigint,
    statregime bigint,
    unitcode character varying(1) DEFAULT NULL::character varying,
    period bigint,
    tradevalue double precision,
    weight double precision,
    quantity bigint
);

ALTER TABLE raw_comext.yearly OWNER TO rdb;

--
-- Name: raw_comext.yearly_template; Type: TABLE; Schema: tradeflows; Owner: rdb
--

CREATE TABLE raw_comext.yearly_template (
    reportercode bigint,
    partnercode bigint,
    productcode character varying(10) DEFAULT NULL::character varying,
    flowcode bigint,
    statregime bigint,
    period bigint,
    tradevalue double precision,
    weight double precision,
    quantity bigint
);

ALTER TABLE raw_comext.yearly_template OWNER TO rdb;


