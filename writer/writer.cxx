#include "vtkRectilinearGrid.h"
#include "vtkDoubleArray.h"
#include "vtkFloatArray.h"
#include "vtkDoubleArray.h"
#include "vtkIntArray.h"
#include "vtkLongArray.h"
#include "vtkPointData.h"
#include "vtkCellData.h"
#include "vtkSmartPointer.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkXMLWriter.h"
#include "vtkXMLRectilinearGridWriter.h"
#include "vtkXMLPRectilinearGridWriter.h"
#include "vtkRectilinearGridAlgorithm.h"
#include "vtkTemplateAliasMacro.h"
#include "vtkMultiProcessController.h"
#include "vtkExtentTranslator.h"

#include <stdio.h>

class vtkExtentModifier : public vtkRectilinearGridAlgorithm
{
public:
  static vtkExtentModifier* New();
#if VTK_MAJOR_VERSION < 6
  vtkTypeRevisionMacro(vtExtentModifier,vtkRectilinearGridAlgorithm);
#else
 vtkTypeMacro(vtkExtentModifier,vtkRectilinearGridAlgorithm);
#endif

  int extent[6];

protected:
  vtkExtentModifier();
  ~vtkExtentModifier();

  virtual int RequestData(
			  vtkInformation* request,
			  vtkInformationVector** InputVector,
			  vtkInformationVector* outputVector);
};

#if VTK_MAJOR_VERSION < 6
  vtkCxxRevisionMacro(vtkExtentModifier, "$Revision: 0.5$");
#endif
  vtkStandardNewMacro(vtkExtentModifier);

  vtkExtentModifier::vtkExtentModifier(){}
  vtkExtentModifier::~vtkExtentModifier(){}

  int vtkExtentModifier::RequestData(
		      vtkInformation* vtkNotUsed(request),
		      vtkInformationVector **inputVector,
		      vtkInformationVector* outputVector )
{
  vtkInformation* outInfo=outputVector->GetInformationObject(0);
  vtkRectilinearGrid* output= vtkRectilinearGrid::SafeDownCast(outInfo->Get(vtkDataObject::DATA_OBJECT() ) );
  vtkRectilinearGrid* input= vtkRectilinearGrid::GetData(inputVector[0]);

  output->ShallowCopy(input);
  output->SetExtent(this->extent);

  return 1;
}

extern "C" {

  void create_vtk_grid(vtkRectilinearGrid *&grid, int dims[3], int extent[6],
		       void* x, void* y, void* z, int datatype) {

    grid = vtkRectilinearGrid::New();
    int gdims[3] ={20,20,20};
    int gextent[6] ={0,20,0,20,0,20};
    grid->SetExtent(gextent);
    grid->SetDimensions(dims);
    grid->SetExtent(extent);

    vtkSmartPointer<vtkDataArray> coords[3];
    void* data[3] = {x, y, z};

    switch(datatype) {
      vtkTemplateAliasMacro(
	for (int k=0; k<3; ++k) {
	  coords[k] = vtkDataArray::CreateDataArray(datatype);
	  coords[k]->SetNumberOfComponents(1);
	  coords[k]->SetNumberOfTuples(dims[k]);
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

  

  void write_vtk_grid(vtkRectilinearGrid *&grid, char* name, int npieces, int piece) {
    vtkMultiProcessController* contr=vtkMultiProcessController::GetGlobalController();
    vtkXMLWriter* writer;
    if (npieces > 1) {
      int extent[6]={0,20,0,10,0,0};
      writer = (vtkXMLWriter*) vtkXMLPRectilinearGridWriter::New();
      vtkExtentModifier* em = vtkExtentModifier::New();
  em->SetInputData(grid);
      grid->GetExtent(em->extent);
  grid->SetExtent(extent);
  std::cout << contr->GetNumberOfProcesses() << " " << contr->GetLocalProcessId()<<std::endl;
      writer->SetInputConnection(em->GetOutputPort());
      ((vtkXMLPRectilinearGridWriter*) writer)->SetController(contr);
      ((vtkXMLPRectilinearGridWriter*) writer)->SetNumberOfPieces(npieces);
      ((vtkXMLPRectilinearGridWriter*) writer)->SetStartPiece(piece);
      ((vtkXMLPRectilinearGridWriter*) writer)->SetEndPiece(piece);
      ((vtkXMLPRectilinearGridWriter*) writer)->SetWriteSummaryFile(piece == 0);
    } else {
      writer = (vtkXMLWriter*) vtkXMLRectilinearGridWriter::New();
      writer->SetInputData(grid);
    }
    writer->SetFileName(name);
    writer->Write();

    if (contr) contr->Barrier();
  }

  void destroy_vtk_grid(vtkRectilinearGrid *&grid) {
    grid->Delete();
  }

}
