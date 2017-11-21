import pymysql
import sqlalchemy
from sqlalchemy import create_engine
from sqlalchemy import Table, Column, Integer, Numeric, String, Text, DateTime, Boolean, ForeignKey, Float
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import sessionmaker, relationship, backref
from datetime import datetime

def connectDB():
    try:
        engine = create_engine("mysql+pymysql://db:ps@127.0.0.1:3306/b_dna")
    except sqlalchemy.exc.DatabaseError:
        print "Can't connect mysql."
    return engine

# Return both engine and session instances.
def createSession():
    engine = connectDB()
    Session = sessionmaker(bind=engine)
    session = Session()
    return engine, session


Base = declarative_base()

class Gene(Base):
	__tablename__ = 'gene'

	id = Column(Integer, primary_key=True)
	name = Column(String(50), nullable=False, unique=True)

class GeneAlias(Base):
	__tablename__ = 'gene_alias'

	id = Column(Integer, primary_key=True)
	gene_id = Column(Integer, ForeignKey('gene.id'))
	alias_name = Column(String(50), nullable=False)


class Species(Base):
	__tablename__ = 'species'

	id = Column(Integer, primary_key=True)
	name = Column(String(50), nullable=False)

class Tissue(Base):
	__tablename__ = 'tissue'

	id = Column(Integer, primary_key=True)
	name = Column(String(50), nullable=False)

class Scientist(Base):
	__tablename__ = 'scientist'

	id = Column(Integer, primary_key=True)
	first_name = Column(String(50), nullable=False)
	last_name = Column(String(50), nullable=False)
	scientist_type_id = Column(Integer, ForeignKey('scientist_type.id'))

class ScientistType(Base):
	__tablename__ = 'scientist_type'

	id = Column(Integer, primary_key=True)
	type = Column(String(50), nullable=False)

class ScientistPolicy(Base):
	__tablename__ = 'scientist_policy'

	id = Column(Integer, primary_key=True)
	scientist_id = Column(Integer, ForeignKey('scientist.id'), nullable=False)
	scientist_type_id = Column(Integer, ForeignKey('scientist_type.id'), nullable=False)

class Experiment(Base):
	__tablename__ = 'experiment'

	id = Column(Integer, primary_key=True)
	description = Column(Text, nullable=False)
	date = Column(DateTime(), nullable=False)
	species_id = Column(Integer, ForeignKey('species.id'))
	tissue_id = Column(Integer, ForeignKey('tissue.id'))
	comments = Column(Text)
	scientist_id = Column(Integer, ForeignKey('scientist.id'))

class GeneExpression(Base):
	__tablename__ = 'gene_expression'

	id = Column(Integer, primary_key=True)
	gene_id = Column(Integer, ForeignKey('gene.id'), nullable=False)
	expression = Column(Float, nullable=False)
	experiment_id = Column(Integer, ForeignKey('experiment.id'), nullable=False)
	condition_id = Column(Integer, ForeignKey('condition.id'), nullable=False)
	# tracking where the gene comes from in a specific file, e.g. creb/creb1
	#file_geneName = Column(String(50))


class Condition(Base):
	__tablename__ = 'condition'

	id = Column(Integer, primary_key=True)
	name = Column(String(200), nullable=False)


class Compound(Base):
	__tablename__ = 'compound'

	id = Column(Integer, primary_key=True)
	name = Column(String(50), nullable=False)


engine, session = createSession()
Base.metadata.create_all(engine)
