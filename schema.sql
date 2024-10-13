-- acquisition. time id
create table acq (
  param_id integer, -- join to session-consitent settings
  AcqTime timestamp,
  SubID text,
  iPAT text,
  B0Shim text,
  Operator text
);

-- acq params that should match across sessions
create table acq_param (
  is_ideal boolean,
  Project text,
  SequenceName text,
  -- TODO: should this be json blob? to extend easier?
  SequenceType text,
  PED text,
  TR numeric,
  TE numeric,
  Matrix text,
  PixelResol text,
  BWP text,
  BWPPE text,
  FA text,
  TA text,
  FoV text,
  MB text,
  FoVShift text,
  SlcLeakBlk text,
  SeqMode text,
  Coil text,
  ShimMode text
);
