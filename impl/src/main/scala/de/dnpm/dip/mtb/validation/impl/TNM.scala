package de.dnpm.dip.mtb.validation.impl



object TNM
{

  // Sources for regex derivation:
  // - https://claritynlp.readthedocs.io/en/latest/developer_guide/algorithms/tnm_stage_finder.html
  // - Value list in Excel sheet found from BfArM (see unit tests of this module)

  val prefix        = "(c|p|yc|yp|r|rp|rc|a)"
  val subsite       = "[a-d]"
  val multiplicity  = "(\\d|m)"
  val suffix        = "(LAMN|DCIS|LCIS|Paget)"
  val tGroupPattern = s"($prefix)?(T[0-4X]|Ta|Tis)((?<!Ta)$subsite|(?<=T[34])e|\\(?mi\\)?)?(\\(?$multiplicity\\)?)?(\\+)?((?<=Tis)\\($suffix\\))?".r
  val nGroupPattern = s"($prefix)?(N[0-3X])($subsite|\\(?mi\\)?)?(\\(\\d/\\d\\))?(\\((i|mol)[\\+-]\\))?(\\(sn\\))?".r
  val mGroupPattern = s"($prefix)?(M[01X])($subsite(\\($multiplicity\\))?)?(\\(cy\\+\\))?(\\((i|mol)\\+\\))?".r
}
