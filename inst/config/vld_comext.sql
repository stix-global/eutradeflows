-- Database structure of the validated comext data
--
-- To load this table in the database, use the R function:
--   eutradeflows::createdbstructure(sqlfile = "vld_comext.sql", dbname = "test")
--

--
-- Table structure for table `vld_comext_product`
--
DROP TABLE IF EXISTS `vld_comext_product`;
CREATE TABLE `vld_comext_product` (
  `productcode` varchar(10) COLLATE utf8_unicode_ci DEFAULT NULL,
  `productdescription` text COLLATE utf8_unicode_ci,
  UNIQUE KEY `productcode` (`productcode`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;


--
-- Table structure for table `vld_comext_unit`
--
DROP TABLE IF EXISTS `vld_comext_unit`;
CREATE TABLE `vld_comext_unit` (
  `productcode` varchar(10) COLLATE utf8_unicode_ci DEFAULT NULL,
  `unitcode` varchar(1) COLLATE utf8_unicode_ci DEFAULT NULL,
  `periodstart` int DEFAULT NULL,
  `periodend` int DEFAULT NULL,
  KEY `productcode` (`productcode`),
  KEY `unitcode` (`unitcode`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;


--
-- Table structure for table `vld_comext_unit_description`
--
DROP TABLE IF EXISTS `vld_comext_unit_description`;
CREATE TABLE `vld_comext_unit_description` (
  `unitcode` varchar(1) COLLATE utf8_unicode_ci DEFAULT NULL,
  `unitdescription` text COLLATE utf8_unicode_ci,
  UNIQUE KEY `unitcode` (`unitcode`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;


--
-- Table structure for table `vld_comext_reporter`
--
DROP TABLE IF EXISTS `vld_comext_reporter`;
CREATE TABLE `vld_comext_reporter` (
  `reportercode` int DEFAULT NULL,
  `reporter` text COLLATE utf8_unicode_ci,
  UNIQUE KEY `reportercode` (`reportercode`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;


--
-- Table structure for table `vld_comext_partner`
--
DROP TABLE IF EXISTS `vld_comext_partner`;
CREATE TABLE `vld_comext_partner` (
  `partnercode` int DEFAULT NULL,
  `partner` text COLLATE utf8_unicode_ci,
  UNIQUE KEY `partnercode` (`partnercode`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;


--
-- Table structure for table `vld_comext_price`
--   yearly global prices in euros per quantity unit (generaly m3)
-- 
DROP TABLE IF EXISTS `vld_comext_price`;
CREATE TABLE `vld_comext_price` (
  `productcode` varchar(10) COLLATE utf8_unicode_ci DEFAULT NULL,
  `flowcode` int DEFAULT NULL,
  `period` int DEFAULT NULL,
  `lowerprice`  double DEFAULT NULL,
  `medianprice`   double DEFAULT NULL,
  `upperprice`  double DEFAULT NULL,
  `averageprice`  double DEFAULT NULL,
  `weightedaverageprice` double DEFAULT NULL, 
  KEY `productcode` (`productcode`),
  KEY `flowcode` (`flowcode`),
  KEY `period` (`period`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;


--
-- Table structure for table `vld_comext_pricew`
--   yearly global prices in euros per Ton
--
-- this table is different than vld_comext_price because it stores
-- price per weight and they are not constructed from the same data.
-- Rows of missing data differ between the weight and quantity columns.
--
DROP TABLE IF EXISTS `vld_comext_pricew`;
CREATE TABLE `vld_comext_pricew` (
  `productcode` varchar(10) COLLATE utf8_unicode_ci DEFAULT NULL,
  `flowcode` int DEFAULT NULL,
  `year` int DEFAULT NULL,
  `unitcode` varchar(1) COLLATE utf8_unicode_ci DEFAULT NULL,
  `lowerprice` double DEFAULT NULL,
  `medianprice` double DEFAULT NULL,
  `upperprice` double DEFAULT NULL,
  `averageprice` double DEFAULT NULL,
  `weightedaverageprice` double DEFAULT NULL,
  `lowerpricew` double DEFAULT NULL,
  `medianpricew` double DEFAULT NULL,
  `upperpricew` double DEFAULT NULL,
  `averagepricew` double DEFAULT NULL,
  `weightedaveragepricew` double DEFAULT NULL,
  `lowerconversion` double DEFAULT NULL,
  `medianconversion` double DEFAULT NULL,
  `upperconversion` double DEFAULT NULL,
  `averageconversion` double DEFAULT NULL,
  `weightedaverageconversion` double DEFAULT NULL,
  KEY `productcode` (`productcode`),
  KEY `flowcode` (`flowcode`),
  KEY `year` (`year`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;


--
-- Table structure for table `vld_comext_cv`
--   yearly global conversion factors
-- 
DROP TABLE IF EXISTS `vld_comext_cv`;
CREATE TABLE `vld_comext_cv` (
  `productcode` varchar(10) COLLATE utf8_unicode_ci DEFAULT NULL,
  `flowcode` int DEFAULT NULL,
  `period` int DEFAULT NULL,
  `medianconversion` double DEFAULT NULL,
  KEY `productcode` (`productcode`),
  KEY `flowcode` (`flowcode`),
  KEY `period` (`period`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;


--
-- Table structure for table `vld_comext_monthly_template`
--
DROP TABLE IF EXISTS `vld_comext_monthly_template`;
CREATE TABLE `vld_comext_monthly_template` (
  `reportercode` int DEFAULT NULL,
  `partnercode` int DEFAULT NULL,
  `productcode` varchar(10) COLLATE utf8_unicode_ci DEFAULT NULL,
  `unitcode` varchar(1) COLLATE utf8_unicode_ci DEFAULT NULL,
  `flowcode` int DEFAULT NULL,
  `statregime` int DEFAULT NULL,
  `period` int DEFAULT NULL,
  `flag` int DEFAULT NULL,
  `tradevalue` double DEFAULT NULL,
  `weight` double DEFAULT NULL,
  `quantity` double DEFAULT NULL,
  `quantityraw` double DEFAULT NULL,
  `quantity_cf` double DEFAULT NULL,
  `quantity_up` double DEFAULT NULL,
  KEY `reportercode` (`reportercode`),
  KEY `partnercode` (`partnercode`),
  KEY `productcode` (`productcode`),
  KEY `flowcode` (`flowcode`),
  KEY `period` (`period`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

