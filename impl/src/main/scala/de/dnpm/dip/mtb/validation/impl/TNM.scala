package de.dnpm.dip.mtb.validation.impl



object TNM
{

  // Source for regex derivation: https://claritynlp.readthedocs.io/en/latest/developer_guide/algorithms/tnm_stage_finder.html

  val prefix        = "(c|p|yc|yp|r|rp|a)"
  val subsite       = "[a-d]"
  val tGroupPattern = s"($prefix)?(T[0-4X]|Tis)($subsite)?(\\((\\d|m)\\))?(\\+)?".r
  val nGroupPattern = s"($prefix)?(N[0-3X])($subsite|\\(mi\\))?(\\(\\d/\\d\\))?(\\(i[\\+-]\\)|\\(mol[\\+-]\\))?(\\(sn\\))?".r
  val mGroupPattern = s"(p)?(M[01X])($subsite)?(\\(cy\\+\\))?(\\(i\\+\\)|\\(mol\\+\\))?".r

}
