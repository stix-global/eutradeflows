--
-- Name: tradeflows; Type: SCHEMA; Schema: -; Owner: rdb
--

CREATE SCHEMA vld_comext;

ALTER SCHEMA vld_comext OWNER TO rdb;

--
-- Name: vld_comext.monthly_template; Type: TABLE; Schema: tradeflows; Owner: rdb
--

CREATE TABLE vld_comext.monthly_template (
    reportercode bigint,
    reporteriso character varying(2) DEFAULT NULL::character varying,
    partnercode bigint,
    partneriso character varying(2) DEFAULT NULL::character varying,
    tradetype character varying(1) DEFAULT NULL::character varying,
    productcode character varying(10) DEFAULT NULL::character varying,
    unitcode character varying(1) DEFAULT NULL::character varying,
    flowcode bigint,
    statregime bigint,
    period bigint,
    flag bigint,
    tradevalue double precision,
    weight double precision,
    quantity double precision,
    quantityraw double precision
);


ALTER TABLE vld_comext.monthly_template OWNER TO rdb;

--
-- Name: vld_comext.partner; Type: TABLE; Schema: tradeflows; Owner: rdb
--

CREATE TABLE vld_comext.partner (
    partnercode bigint,
    partner text
);


ALTER TABLE vld_comext.partner OWNER TO rdb;

--
-- Name: vld_comext.priceconversion; Type: TABLE; Schema: tradeflows; Owner: rdb
--

CREATE TABLE vld_comext.priceconversion (
    productcode character varying(10) DEFAULT NULL::character varying,
    flowcode bigint,
    year bigint,
    unitcode character varying(1) DEFAULT NULL::character varying,
    lowerprice double precision,
    medianprice double precision,
    upperprice double precision,
    averageprice double precision,
    weightedaverageprice double precision,
    lowerpricew double precision,
    medianpricew double precision,
    upperpricew double precision,
    averagepricew double precision,
    weightedaveragepricew double precision,
    lowerconversion double precision,
    medianconversion double precision,
    upperconversion double precision,
    averageconversion double precision,
    weightedaverageconversion double precision
);


ALTER TABLE vld_comext.priceconversion OWNER TO rdb;

--
-- Name: vld_comext.product; Type: TABLE; Schema: tradeflows; Owner: rdb
--

CREATE TABLE vld_comext.product (
    productcode character varying(10) DEFAULT NULL::character varying,
    productdescription text
);


ALTER TABLE vld_comext.product OWNER TO rdb;

--
-- Name: vld_comext.reporter; Type: TABLE; Schema: tradeflows; Owner: rdb
--

CREATE TABLE vld_comext.reporter (
    reportercode bigint,
    reporter text
);


ALTER TABLE vld_comext.reporter OWNER TO rdb;

--
-- Name: vld_comext.unit; Type: TABLE; Schema: tradeflows; Owner: rdb
--

CREATE TABLE vld_comext.unit (
    productcode character varying(10) DEFAULT NULL::character varying,
    unitcode character varying(1) DEFAULT NULL::character varying,
    periodstart bigint,
    periodend bigint
);


ALTER TABLE vld_comext.unit OWNER TO rdb;

--
-- Name: vld_comext.unit_description; Type: TABLE; Schema: tradeflows; Owner: rdb
--

CREATE TABLE vld_comext.unit_description (
    unitcode character varying(1) DEFAULT NULL::character varying,
    unitdescription text
);

