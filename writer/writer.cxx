#include "vtkRectilinearGrid.h"
#include "vtkDoubleArray.h"
#include "vtkFloatArray.h"
#include "vtkDoubleArray.h"
#include "vtkIntArray.h"
#include "vtkLongArray.h"
#include "vtkPointData.h"
#include "vtkCellData.h"
#include "vtkSmartPointer.h"
#include "vtkObjectFactory.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkXMLWriter.h"
#include "vtkXMLRectilinearGridWriter.h"
#include "vtkXMLStructuredDataWriter.h"
#include "vtkXMLPRectilinearGridWriter.h"
#include "vtkRectilinearGridAlgorithm.h"
#include "vtkTemplateAliasMacro.h"
#include "vtkStreamingDemandDrivenPipeline.h"
#include "vtkVersion.h"
#include "vtkErrorCode.h"

#include <mpi.h>
#include <stdio.h>

#if VTK_MAJOR_VERSION >5
vtkMPIController* generic;
#endif

class myPieceWriter : public vtkXMLRectilinearGridWriter {
public:
  static myPieceWriter* New();
#if VTK_MAJOR_VERSION < 6
  vtkTypeRevisionMacro(myPieceWriter, vtkXMLRectilinearGridWriter);
#else
  vtkTypeMacro(myPieceWriter, vtkXMLRectilinearGridWriter);
#endif
protected:
  myPieceWriter();
  ~myPieceWriter();
  vtkDataArray* CreateExactCoordinates(vtkDataArray*, int);
  void CalculateSuperclassFraction(float*);
  void SetInputUpdateExtent(int);
};

class myWriter : public vtkXMLPRectilinearGridWriter {
public:
  static myWriter* New();
#if VTK_MAJOR_VERSION < 6
  vtkTypeRevisionMacro(myWriter, vtkXMLPRectilinearGridWriter);
#else
  vtkTypeMacro(myWriter, vtkXMLPRectilinearGridWriter);
#endif
  vtkGetVectorMacro(GlobalExtent, int, 6);
  vtkSetVectorMacro(GlobalExtent, int, 6);
  void UpdateGlobalExtent(int local_extent[6]);
protected:
  myWriter();
  ~myWriter();
  void WritePrimaryElementAttributes(ostream &, vtkIndent);
  myPieceWriter* CreateStructuredPieceWriter();
  vtkXMLWriter* CreatePieceWriter(int);
  void WritePPieceAttributes(int);
  int GlobalExtent[6];
  int* Extent;
};


#if VTK_MAJOR_VERSION < 6
vtkCxxRevisionMacro(myWriter, "$Revision: 1.0$");
#endif
vtkStandardNewMacro(myWriter);

myWriter::myWriter(){this->Extent = NULL;}
myWriter::~myWriter(){ if (Extent) delete[] Extent;}

void myWriter::UpdateGlobalExtent(int local_extent[6]) {

#ifndef __vtkMPIController_h

  this->Extent = new int[6*this->GetNumberOfPieces()];
  MPI_Gather( local_extent, 6, MPI_INT, 
	      this->Extent, 6, MPI_INT,
	      0, MPI_COMM_WORLD);

  this->SetGlobalExtent(this->Extent);
  for (int i=1; i<this->GetNumberOfPieces();++i) {
    for (int j=0;j<3;++j) {
      this->GlobalExtent[2*j] = std::min(this->GlobalExtent[2*j],
					 this->Extent[6*i+2*j]);
      this->GlobalExtent[2*j+1] = std::max(this->GlobalExtent[2*j+1],
					 this->Extent[6*i+2*j+1]);   
    }
  }

#else
  int send_buffer[3], recv_buffer[3];
  int op[2] = {vtkCommunicator::MIN_OP, vtkCommunicator::MAX_OP};
  for(int i=0;i<2;++i) {
    for (int j=0;j<3;++j) {
      send_buffer[j] = local_extent[2*j+i];
    }
    this->Controller->Reduce(send_buffer, recv_buffer, 3, op[i], 0);

    for (int j=0;j<3;++j) {
      this->GlobalExtent[2*j+i]=recv_buffer[j];
    }
  }
#endif
}

vtkXMLWriter* myWriter::CreatePieceWriter(int vtkNotUsed(index)){

  myPieceWriter* pWriter = this->CreateStructuredPieceWriter();

  return pWriter;
}

myPieceWriter* myWriter::CreateStructuredPieceWriter()
{  
  // Create the writer for the piece.
  myPieceWriter* pWriter = myPieceWriter::New();
  pWriter->SetInputConnection(this->GetInputConnection(0, 0));
  return pWriter;
}

void myWriter::WritePrimaryElementAttributes(ostream &os, vtkIndent indent)
{
  this->GetExecutive()->GetInputInformation(0, 0)->Set(
				     vtkStreamingDemandDrivenPipeline::WHOLE_EXTENT(),
				       this->GlobalExtent, 6);
  this->Superclass::WritePrimaryElementAttributes(os, indent);
}

void myWriter::WritePPieceAttributes(int index)
{
  this->WriteVectorAttribute("Extent", 6, &(this->Extent[6*index]));
  if (this->ErrorCode == vtkErrorCode::OutOfDiskSpaceError)
    {
    return;
    }
  this->Superclass::Superclass::Superclass::WritePPieceAttributes(index);
}

#if VTK_MAJOR_VERSION < 6
vtkCxxRevisionMacro(myPieceWriter, "$Revision: 1.0$");
#endif
vtkStandardNewMacro(myPieceWriter);

myPieceWriter::myPieceWriter(){}
myPieceWriter::~myPieceWriter(){}


vtkDataArray* myPieceWriter::CreateExactCoordinates(vtkDataArray* a, int xyz) {
  if (!a) 
    {
      return vtkFloatArray::New();
    }
  
  a->Register(0);
  return a;
}

void myPieceWriter::CalculateSuperclassFraction(float* fractions) {

  std::cout << "hi!" <<std::endl;

  int extent[6];
  this->GetInput()->GetExtent(extent);

  int dims[3] = {extent[1]-extent[0]+1,
                 extent[3]-extent[2]+1,
                 extent[5]-extent[4]+1};
  
  // The amount of data written by the superclass comes from the
  // point/cell data arrays.
  vtkIdType superclassPieceSize = 
    (this->GetInput()->GetPointData()->GetNumberOfArrays()*dims[0]*dims[1]*dims[2]+
     this->GetInput()->GetCellData()->GetNumberOfArrays()*(dims[0]-1)*(dims[1]-1)*(dims[2]-1));
  
  // The total data written includes the coordinate arrays.
  vtkIdType totalPieceSize =
    superclassPieceSize + dims[0] + dims[1] + dims[2];
  if(totalPieceSize == 0)
    {
    totalPieceSize = 1;
    }
  fractions[0] = 0;
  fractions[1] = fractions[0] + float(superclassPieceSize)/totalPieceSize;
  fractions[2] = 1;
}

void myPieceWriter::SetInputUpdateExtent(int piece)
{
  vtkInformation* inInfo = 
    this->GetExecutive()->GetInputInformation(0, 0);
  inInfo->Set(
    vtkStreamingDemandDrivenPipeline::UPDATE_EXTENT(), 
    this->GetInput()->GetExtent(),
    6);
}

extern "C" {

  void initialize_vtk(){
#ifdef __vtkMPIController_h
    vtkMPICommunicator* comm=vtkMPICommunicator::New();
    generic=vtkMPIController::New() ;
    generic->SetCommunicator(comm->GetWorldCommunicator());
    generic->SetGlobalController(generic);
#endif
  }

  void create_vtk_grid(vtkRectilinearGrid *&grid, int dims[3], int extent[6],
		       void* x, void* y, void* z, int datatype) {

    grid = vtkRectilinearGrid::New();
    grid->SetDimensions(dims);
    grid->SetExtent(extent);

    vtkSmartPointer<vtkDataArray> coords[3];

    switch(datatype) {
    case(VTK_DOUBLE):
      for (int k=0; k<3; ++k) {
	coords[k] = static_cast<vtkDataArray*>(vtkDoubleArray::New());
	coords[k]->SetNumberOfComponents(1);
	coords[k]->SetNumberOfTuples(dims[k]);
      }
      break;
    case(VTK_FLOAT):
      for (int k=0; k<3; ++k) {
	coords[k] = static_cast<vtkDataArray*>(vtkFloatArray::New());
	coords[k]->SetNumberOfComponents(1);
	coords[k]->SetNumberOfTuples(dims[k]);
      }
      break;
    }
      
    void* data[3] = {x, y, z};

    switch(datatype) {
      vtkTemplateAliasMacro(
	for (int k=0; k<3; ++k) {
	  for (int i=0; i<dims[k]; ++i) {
	    coords[k]->SetComponent(i, 0, static_cast<VTK_TT*>(data[k])[i]);
	  }
	});
    }		    
    grid->SetXCoordinates(coords[0]);
    grid->SetYCoordinates(coords[1]);
    grid->SetZCoordinates(coords[2]);
  }

  void add_point_data(vtkRectilinearGrid *&grid, char* name, int ncomponents,
		      void* raw_data, int datatype) {

    int dims[3];
    grid->GetDimensions(dims);
    
    vtkDataArray* vtk_data = vtkDataArray::CreateDataArray(datatype);
    vtk_data->SetName(name);
    vtk_data->SetNumberOfComponents(ncomponents);
    vtk_data->SetNumberOfTuples(grid->GetNumberOfPoints());
    int count=0;
    switch(datatype) {
      vtkTemplateAliasMacro(
	for (int i=0; i<dims[0]; ++i) {
	  for (int j=0; j<dims[1]; ++j) {
	    for (int k=0; k<dims[2]; ++k) {
	      for (int c=0; c<ncomponents; ++c) {
		int offset=k+j*dims[2]+i*dims[2]*dims[1];
		vtk_data->SetComponent(count,c, 
				       static_cast<VTK_TT*>(raw_data)[offset*ncomponents+c]);
	      }
	      ++count;
	    }
	  }
	});
    }
    grid->GetPointData()->AddArray(vtk_data);
    vtk_data->Delete();
  }

  void add_cell_data(vtkRectilinearGrid *&grid, char* name, int ncomponents,
		     int* dims, void* raw_data, int datatype) {
    
    vtkSmartPointer<vtkDataArray> vtk_data = vtkDataArray::CreateDataArray(datatype);
    vtk_data->SetName(name);
    vtk_data->SetNumberOfComponents(ncomponents);
    vtk_data->SetNumberOfTuples(grid->GetNumberOfCells());
    int count=0;
    int offset=0;
    switch(datatype) {
      vtkTemplateAliasMacro(
			    for (int i=0; i<dims[0]; ++i) {
			      for (int j=0; j<dims[1]; ++j) {
				for (int k=0; k<dims[2]; ++k) {
				  for (int c=0; c<ncomponents; ++c) {
				    int offset=k+j*dims[2]+i*dims[2]*dims[1];
				    vtk_data->SetComponent(count,c, 
							   static_cast<VTK_TT*>(raw_data)[offset*ncomponents+c]);
				  }
				  ++count;
				}
			      }
			    });
	}
    grid->GetCellData()->AddArray(vtk_data);
  }

  void write_vtk_grid(vtkRectilinearGrid *&grid, char* name, int npieces, int piece) {
    int* global_extent=NULL;
    if (npieces > 1) {
      myWriter* writer = myWriter::New();
#if VTK_MAJOR_VERSION < 6
      writer->SetInput(grid);
#else
      writer->SetInputData(grid);
#endif
#ifdef __vtkMPIController_h
      writer->SetController(generic);
#endif
      writer->SetNumberOfPieces(npieces);
      writer->SetStartPiece(piece);
      writer->SetEndPiece(piece);
      writer->SetWriteSummaryFile(piece == 0);
      writer->SetFileName(name);
      if (global_extent) {
	writer->SetGlobalExtent(global_extent);
      } else {
	writer->UpdateGlobalExtent(grid->GetExtent());
      }
      writer->Write();
      writer->Delete();
    } else {
      vtkXMLRectilinearGridWriter* writer = vtkXMLRectilinearGridWriter::New();
#if VTK_MAJOR_VERSION < 6
      writer->SetInput(grid);
#else
      writer->SetInputData(grid);
#endif
      writer->SetFileName(name);
      writer->Write();
      writer->Delete();
    }
  }

  void destroy_vtk_grid(vtkRectilinearGrid *&grid) {
    grid->Delete();
  }

}
