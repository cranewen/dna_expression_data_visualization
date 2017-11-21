import os
import re
import xlsxwriter
import csv
import os.path
from numpy.core.defchararray import upper

class ConditionParser:
	def __init__(self, path):
		self.path = path

		self.conditions = []

	def getConditions(self, path):
		directoryNames = self.getDirectoryNames()
		# conditions = []
		# whereInTheFile = [] # stores the null data
		
		for d in directoryNames:
			for fileName in os.listdir(d):
				filePathName = d + "/" + fileName
				with open(filePathName, 'r') as f:
					data = csv.reader(f)
					row1 = next(data)
					for i in row1:
						self.conditions.append(i.strip().upper())
						# if (i == ''):
						# 	whereInTheFile.append(filePathName)
							# print filePathName
		self.conditions = filter(None, list(set(self.conditions)))


	def getDirectoryNames(self):
		directoryNames = []
		lenOfPath = len(self.path)
		for dirNames in os.walk(self.path):
			for dir in dirNames:
				lenOfDir = len(dir)
				if os.path.isdir(str(dir)) and dir != self.path:
					directoryNames.append(dir)
		return directoryNames

