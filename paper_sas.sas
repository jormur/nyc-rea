/* Read CSV */
proc import datafile='/home/u63590408/term_project/NYC.csv'
  out=NYC
  dbms=csv REPLACE;
run;

/* Summary Statistics */

/* H1: Newer buildings (as measured by year built) sell for higher prices, controlling for other factors */

/* Avg Sale Price by decade of year built, excluding zeroes and NAs */
proc sql;
create table avg_sale_price_by_year as
select distinct floor(YEAR_BUILT/10)*10 as YEAR_BUILT,
       mean(SALE_PRICE) as mean
from NYC
where SALE_PRICE ne 0 and not missing(SALE_PRICE)
          and not missing(YEAR_BUILT)
group by floor(YEAR_BUILT/10)*10;
quit;

/* Avg Price per square foot by decade of year built, excluding zeroes and NAs */
proc sql;
create table avg_price_per_sqft_by_year as
select distinct floor(YEAR_BUILT/10)*10 as YEAR_BUILT,
       mean(SALE_PRICE/GROSS_SQUARE_FEET) as mean
from NYC
where SALE_PRICE ne 0 and not missing(SALE_PRICE)
          and not missing(YEAR_BUILT)
group by floor(YEAR_BUILT/10)*10;
quit;

/* H2: Properties in neighborhoods with more commercial units sell for higher prices, controlling for other factors */

/* Avg Sale Price by building use by borough, excluding zeroes and NAs */
proc sql;
create table avg_sale_price_by_borough as
select BOROUGH, unit_type,
       mean(SALE_PRICE) as avg_sale_price
from NYC
where unit_type ne 'Other' and SALE_PRICE ne .
group by BOROUGH, unit_type
order by BOROUGH, unit_type;
quit;

/* Avg PPSF by building use by borough, excluding zeroes and NAs */
proc sql;
create table avg_sale_price_by_borough as
select BOROUGH, unit_type,
       mean(SALE_PRICE/GROSS_SQUARE_FEET) as avg_sale_price
from NYC
where unit_type ne 'Other' and SALE_PRICE ne .
group by BOROUGH, unit_type
order by BOROUGH, unit_type;
quit;

/* H3: Larger lot sizes increase sales prices, controlling for other factors */

/* Compare land square footage to gross square footage by borough, excluding zeroes and NAs */
proc sql;
create table mean_building_to_land_ratio as
select BOROUGH,
       mean(BUILDING_TO_LAND_RATIO) as mean
from NYC
where BUILDING_TO_LAND_RATIO ne .
group by BOROUGH;
quit;

/* Compare land square footage to gross square footage by borough and building use */
proc sql;
create table mean_building_to_land_ratio as
select BOROUGH, unit_type,
       mean(BUILDING_TO_LAND_RATIO) as mean
from NYC
where BUILDING_TO_LAND_RATIO ne .
group by BOROUGH, unit_type
order by BOROUGH, unit_type;
quit;

/* Avg price per square foot by building to land ratio, excluding zeroes and NAs */
proc sql;
create table avg_price_per_sqft_by_ratio as
select distinct floor(BUILDING_TO_LAND_RATIO*10)/10 as BUILDING_TO_LAND_RATIO,
       mean(SALE_PRICE/GROSS_SQUARE_FEET) as mean
from NYC
where BUILDING_TO_LAND_RATIO ne . and BUILDING_TO_LAND_RATIO between 0 and 2
group by floor(BUILDING_TO_LAND_RATIO*10)/10
order by floor(BUILDING_TO_LAND_RATIO*10)/10;
quit;



/* Regression Analysis */


/* H1: Newer buildings (as measured by year built) sell for higher prices, controlling for other factors */
proc glm data=NYC;
  where not missing(PRICE_PER_SQ_FT) and PRICE_PER_SQ_FT ne .;
  class BOROUGH unit_type;
  model PRICE_PER_SQ_FT = PROPERTY_AGE BOROUGH RESIDENTIAL_UNITS 
                           COMMERCIAL_UNITS LAND_SQUARE_FEET 
                           GROSS_SQUARE_FEET unit_type / solution;
run;


/* H2: Properties in neighborhoods with more commercial units sell for higher prices, controlling for other factors */
proc glm data=NYC;
  where not missing(PRICE_PER_SQ_FT) and PRICE_PER_SQ_FT ne .;
  class BOROUGH unit_type;
  model PRICE_PER_SQ_FT = avg_commercial_units RESIDENTIAL_UNITS 
                           COMMERCIAL_UNITS LAND_SQUARE_FEET 
                           GROSS_SQUARE_FEET / solution;
run;

/* H3: Larger lot sizes increase sales prices, controlling for other factors */
proc glm data=NYC;
  where not missing(PRICE_PER_SQ_FT) and PRICE_PER_SQ_FT ne .;
  class BOROUGH unit_type;
  model PRICE_PER_SQ_FT = BUILDING_TO_LAND_RATIO RESIDENTIAL_UNITS 
                           COMMERCIAL_UNITS LAND_SQUARE_FEET 
                           GROSS_SQUARE_FEET / solution;
run;
