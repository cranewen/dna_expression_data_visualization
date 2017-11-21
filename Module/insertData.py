import pymysql
import sqlalchemy
from sqlalchemy import create_engine
from sqlalchemy import Table, Column, Integer, Numeric, String, Text, DateTime, Boolean, ForeignKey, Float
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import sessionmaker, relationship, backref
from sqlalchemy import func
from datetime import datetime
import sys
import os.path
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from FileTools import geneNamesParser, descriptionParser, conditionParser, geneExpressionParser
from createTables import *
import pandas as pd
import re
from collections import defaultdict
import threading, time

def connectDB():
    try:
        engine = create_engine("mysql+pymysql://db:ps@127.0.0.1:3306/b_dna")
    except sqlalchemy.exc.DatabaseError:
        print "Can't connect mysql."
    return engine

def createSession():
    engine = connectDB()
    Session = sessionmaker(bind=engine)
    session = Session()
    return session

def getGeneNames():
    data = geneNamesParser.GeneNamesParser("/data_path")
    data.getGeneNamesWithAlias(data.getGeneNames())
    geneDict = data.alias_geneName_dict
    genes = []
    for k,v in geneDict.iteritems():
        genes.append(v)
    genes = list(set(genes)) # remove the duplicates
    return genes

def getGeneDict():
    data = geneNamesParser.GeneNamesParser("/data_path")
    geneDict = data.getGeneNamesWithAlias(data.getGeneNames())
    geneDict = data.alias_geneName_dict
    return geneDict

################### Insertion functions start ##########################
def insertGene():
    genes = getGeneNames()
    session = createSession()
    for g in genes:
        x = Gene(name = g)
        session.add(x)
    session.commit()

def insertGeneAlias():
    gene_dict = getGeneDict()
    session = createSession()
    for k,v in gene_dict.iteritems():
        geneId = session.query(Gene.id).filter(Gene.name == v)
        alias = GeneAlias(gene_id = geneId, alias_name = k)
        # SQL equivalent statement1:
        # INSERT INTO gene_alias SET alias_name = k,
        # gene_id = (SELECT id from gene WHERE name = v);
        # SQL equivalent statement2:
        # INSERT INTO gene_alias (gene_id, alias_name)
        # SELECT id, 'v' FROM gene WHERE name = k;
        session.add(alias)
    session.commit()

def insertConditions():
    c = conditionParser.ConditionParser("/data_path")
    c.getConditions(c.path)
    conditions = c.conditions
    session = createSession()
    for con in conditions:
        condition = Condition(name = con)
        session.add(condition)
    session.commit()

def insertExperiments():
    d = descriptionParser.DescriptionParser("/data_path")
    d.getDescritpionDict()
    descriptionsDict = d.description_dict
    session = createSession()
    for key in descriptionsDict.keys():
        speciesId = None
        tissueId = None
        if ('species' in descriptionsDict[key]):
            speciesId = session.query(Species.id).filter(Species.name == descriptionsDict[key]['species'])
        if ('tissue' in descriptionsDict[key]):
            tissueId = session.query(Tissue.id).filter(Tissue.name == descriptionsDict[key]['tissue'])
        experiment = Experiment(description = key.strip(), date = descriptionsDict[key]['date'],
            species_id = speciesId, tissue_id = tissueId)
        session.add(experiment)
    session.commit()

def insertSpecies():
    species = ['RAT', 'MICE', 'MOUSE']
    session = createSession()
    for s in species:
        spec = Species(name = s)
        session.add(spec)
    session.commit()

def insertTissues():
    tissues = ['KIDNEY', 'HEART', 'BRAIN', 'LIVER', 'LUNG', 'RIGHT KIDNEY', 'LEFT KIDNEY', 'EYE']
    session = createSession()
    for t in tissues:
        tiss = Tissue(name = t)
        session.add(tiss)
    session.commit()

def insertGeneExpression():
    geneExpression = geneExpressionParser.GeneExpressionParser("/data_path")
    experiment_dict = geneExpression.getDataDict()
    session = createSession()
    experiment_id = ''
    gene_id = ''
    condition_id = ''
    expression = ''
    for exper, geneNames in experiment_dict.iteritems():
        description = exper
        # insert experiment_id
        # if (description != [] and description != '' and description != None):
        experiment_id = session.query(Experiment.id).filter(Experiment.description == description)
        for gene, geneExpressionData in geneNames.iteritems():
            gene_name = gene
            # insert gene_id
            gene_id = session.query(GeneAlias.gene_id).filter(GeneAlias.alias_name == func.binary(gene_name))
            # loop through dataframe, get single gene expression
            for con in geneExpressionData.columns:
                condition = con
                condition_id = session.query(Condition.id).filter(Condition.name == condition.strip())
                for express in geneExpressionData[condition]:
                    if (not re.search(r"(.+?)[*]", str(express)) and str(express) != 'nan'):
                        expression = float(express)
                        gene_Expression = GeneExpression(gene_id = gene_id, expression = expression, experiment_id = experiment_id,
                        condition_id = condition_id)
                        session.add(gene_Expression)
    session.commit()
    session.close()

# improved version for inserting data, all the data has been changed to FK IDs as integers. Therefore, inserting data without selecting from tables.
def insertGeneExpressionVtwo():
    geneExpression = geneExpressionParser.FinalGeneExpressionDict("/data_path")
    geneExpressionDict = geneExpression.getFinalGeneExpressionDict()
    session = createSession()
    for experiment_id, geneDict in geneExpressionDict.iteritems():
        for gene_id, geneData in geneDict.iteritems():
            for condition_id in geneData.columns:
                for express in geneData[condition_id]:
                    if (not re.search(r"(.+?)[*]", str(express)) and str(express) != 'nan'):
                        expression = float(express)
                        gene_Expression = GeneExpression(gene_id = int(gene_id), expression = expression, experiment_id = int(experiment_id),
                        condition_id = int(condition_id))
                        session.add(gene_Expression)
    session.commit()
    session.close()

# Create threads for running insert functions as a particular order.
def insertProcess():
    insertFuncArray = [insertGene, insertGeneAlias, insertConditions, insertSpecies, insertTissues, insertExperiments, insertGeneExpressionVtwo]
    insertThreads = []
    for i in insertFuncArray:
        t = threading.Thread(target = i, args = ())
        t.start()
        insertThreads.append(t)
    for thread in insertThreads:
        thread.join()

# insertGene()
# insertGeneAlias()
# insertConditions()
# insertSpecies()
# insertTissues()
# insertExperiments()
# insertGeneExpression()
# insertGeneExpressionVtwo()
insertProcess()
