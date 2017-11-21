import os
import re
import xlsxwriter
import csv
import os.path
from numpy.core.defchararray import upper

# GeneNamesParser does parse gene names and descriptions.
# It returns gene alias and description dictionary,
# and gene names and gene alias dictionary.

class GeneNamesParser:
	def __init__(self, path):
		self.path = path

		self.alias_geneName_dict = {}
		self.gene_description_dict = {}

	# Get unique gene names(gene alias), also get gene and description dictionary
	def getGeneNames(self):
		fileNames = []
		geneNames = []
		for folderName in os.listdir(self.path):
			# subPath = ''
			description = folderName
			self.gene_description_dict[description] = []
			folderName = self.path + "/" + folderName
			if os.path.isdir(folderName):
				for fileName in os.listdir(folderName):
					geneName = self.getGeneName(fileName)
					fileNames.append(geneName)
					self.gene_description_dict[description].append(geneName)
		uniqeGeneNames = list(set(fileNames))

		return uniqeGeneNames

	def getGeneName(self,s):
		subString = re.search(r"(s|Data 3-)(.+?)(.csv)", s).group(2)
		return subString


	#Get all the gene names and make dictionaries {gene_alias:gene_name} !!!
	def getGeneNamesWithAlias(self, geneNameList):
		# alias_geneName_dict = {}
		for g in geneNameList:
			tempG = g.upper()
			tempG = re.sub(r'[-]','', tempG)
			tempG = re.sub(r'[ ]','', tempG)
			tempG = re.sub(r'\(.+?\)','', tempG)
			if (tempG == 'CREB' or tempG == 'TGFB'):
				tempG = tempG + '1'
			self.alias_geneName_dict[g] = tempG

		# return alias_geneName_dict

# x = GeneNamesParser("/data_path")
# x.getGeneNames()
# print x.gene_description_dict
