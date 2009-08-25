<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output type="xml"/>
<xsl:output omit-xml-declaration="yes"/>

<xsl:template match="/">
 <xsl:apply-templates select="testsuite"/>
</xsl:template>

<xsl:template match="testsuite">
 <sect1>
  <title>
   <xsl:value-of select="@name"/>
  </title>

  <table frame='all'>
   <title><xsl:value-of select="@name"/> summary</title>
   <tgroup cols='4'>
   <thead>
    <row>
     <entry>Errors</entry>
     <entry>Failures</entry>
     <entry>Tests</entry>
     <entry>Time</entry>
    </row>
   </thead>
   <tbody>
    <row>
     <entry><xsl:value-of select="@errors"/></entry>
     <entry><xsl:value-of select="@failures"/></entry>
     <entry><xsl:value-of select="@tests"/></entry>
     <entry><xsl:value-of select="@time"/></entry>
    </row>
   </tbody>
   </tgroup>
  </table>

  <table frame='all'>
   <title><xsl:value-of select="@name"/> details</title>
   <tgroup cols='3' colsep='1' rowsep='1'>
   <thead>
    <row>
     <entry>Name</entry>
     <entry>Time</entry>
     <entry>Status</entry>
    </row>
   </thead>
   <tbody>

    <xsl:apply-templates select="testcase"/>

	</tbody>
   </tgroup>
  </table>

 </sect1>
</xsl:template>

<xsl:template match="testcase">
 <row>
  <entry><xsl:value-of select="@name"/></entry>
  <entry><xsl:value-of select="@time"/></entry>
  <entry>
   <xsl:choose>
    <xsl:when test="failure">
     <xsl:apply-templates select="failure"/>
    </xsl:when>
	<xsl:otherwise>
	PASSED
	</xsl:otherwise>
   </xsl:choose>
  </entry>
 </row>
</xsl:template>

<xsl:template match="failure">
 FAILURE (<xsl:value-of select="@type"/>)
</xsl:template>

</xsl:stylesheet>
