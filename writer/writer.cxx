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
#include "vtkMPIController.h"
#include "vtkXMLWriter.h"
#include "vtkXMLRectilinearGridWriter.h"
#include "vtkXMLStructuredDataWriter.h"
#include "vtkXMLPRectilinearGridWriter.h"
#include "vtkRectilinearGridAlgorithm.h"
#include "vtkTemplateAliasMacro.h"
#include "vtkStreamingDemandDrivenPipeline.h"
#include "vtkVersion.h"

#include <mpi.h>
#include <stdio.h>

#if VTK_MAJOR_VERSION >5
vtkMPIController* generic;
#endif

class myWriter : public vtkXMLPRectilinearGridWriter {
public:
  static myWriter* New();
#if VTK_MAJOR_VERSION < 6
  vtkTypeRevisionMacro(myWriter, vtkXMLPRectilinearGridWriter);
#else
  vtkTypeMacro(myWriter, vtkXMLPRectilinearGridWriter);
#endif
  vtkGetVectorMacro(GlobalExtent, int, 6);
  void SetGlobalExtent(int extent[6]);
  void Write(int extent[6]);
protected:
  myWriter();
  ~myWriter();
  void WritePrimaryElementAttributes(ostream &, vtkIndent);
  int GlobalExtent[6];
};

#if VTK_MAJOR_VERSION < 6
vtkCxxRevisionMacro(myWriter, "$Revision: 0.5$");
#endif
vtkStandardNewMacro(myWriter);

myWriter::myWriter(){}
myWriter::~myWriter(){}

void myWriter::SetGlobalExtent(int local_extent[6]) {

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
}

void myWriter::Write(int extent[6]) { 
  this->SetGlobalExtent(extent);
  this->Superclass::Write();
}

void myWriter::WritePrimaryElementAttributes(ostream &os, vtkIndent indent)
{

  this->GetInputInformation(0, 0)->Set(vtkStreamingDemandDrivenPipeline::WHOLE_EXTENT(),
				       this->GlobalExtent, 6);
  this->Superclass::WritePrimaryElementAttributes(os, indent);
}

extern "C" {

  void initialize_vtk(){
#if VTK_MAJOR_VERSION >5
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
	    coords[k]->SetComponent(i, 1, static_cast<VTK_TT*>(data[k])[i]);
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
    
    vtkSmartPointer<vtkDataArray> vtk_data = vtkDataArray::CreateDataArray(datatype);
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
		int offset=i+j*dims[0]+k*dims[0]*dims[1];
		vtk_data->SetComponent(count,c, 
				       static_cast<VTK_TT*>(raw_data)[offset*ncomponents+c]);
	      }
	      ++count;
	    }
	  }
	});
    }
    grid->GetPointData()->AddArray(vtk_data);
  }

  void add_cell_data(vtkRectilinearGrid *&grid, char* name, int ncomponents,
		     int* dims, void* raw_data, int datatype) {
    
    vtkSmartPointer<vtkDataArray> vtk_data = vtkDataArray::CreateDataArray(datatype);
    vtk_data->SetName(name);
    vtk_data->SetNumberOfComponents(ncomponents);
    vtk_data->SetNumberOfTuples(grid->GetNumberOfCells());
    int count=0;
    switch(datatype) {
      vtkTemplateAliasMacro(
			    for (int i=0; i<dims[0]; ++i) {
			      for (int j=0; j<dims[1]; ++j) {
				for (int k=0; k<dims[2]; ++k) {
				  for (int c=0; c<ncomponents; ++c) {
				    int offset=i+j*dims[0]+k*dims[0]*dims[1];
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

  void write_vtk_grid(vtkRectilinearGrid *&grid, char* name, int npieces, int piece, int* global_extent) {
    if (npieces > 1) {
      myWriter* writer = myWriter::New();
#if VTK_MAJOR_VERSION < 6
      writer->SetInput(grid);
#else
      writer->SetInputData(grid);
      writer->SetController(generic);
#endif
      writer->SetNumberOfPieces(npieces);
      writer->SetStartPiece(piece);
      writer->SetEndPiece(piece);
      writer->SetWriteSummaryFile(1);
      writer->SetFileName(name);
      writer->Write(grid->GetExtent());
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
