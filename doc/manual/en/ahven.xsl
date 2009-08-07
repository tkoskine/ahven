<?xml version='1.0'?>
<xsl:stylesheet
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    xmlns:hl="java:net.sf.xslthl.ConnectorSaxon6"
    xmlns:xslthl="http://xslthl.sf.net"
    exclude-result-prefixes="xslthl"
    version="1.0">
<xsl:import href="/usr/local/share/xsl/docbook/fo/docbook.xsl"/>
<xsl:import href="fop-new.xsl"/>

<xsl:param name="section.autolabel" select="1"/>
<xsl:param name="toc.section.depth" select="2"/>
<xsl:param name="section.label.includes.component.label" select="1"/>
<xsl:param name="generate.toc"/>
<xsl:param name="process.empty.source.toc" select="1"/>
<xsl:param name="xref.with.number.and.title" select="0"/>
<xsl:param name="bibliography.numbered" select="1"/>

<xsl:param name="formal.title.placement">
figure after
example after
equation after
table before
procedure before
</xsl:param>

<xsl:param name="table.cell.border.thickness" select="'0.5mm'"/>
<xsl:param name="table.frame.border.thickness" select="'0.5mm'"/>

</xsl:stylesheet>
