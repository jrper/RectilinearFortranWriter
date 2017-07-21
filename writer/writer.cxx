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
#include "vtkMultiProcessController.h"
#include "vtkExtentTranslator.h"
#include "vtkStreamingDemandDrivenPipeline.h"
#include "vtkVersion.h"

#include <stdio.h>

class vtkExtentModifier : public vtkRectilinearGridAlgorithm
{
public:
  static vtkExtentModifier* New();
#if VTK_MAJOR_VERSION < 6
  vtkTypeRevisionMacro(vtkExtentModifier,vtkRectilinearGridAlgorithm);
#else
 vtkTypeMacro(vtkExtentModifier,vtkRectilinearGridAlgorithm);
#endif

  int extent[6];
  int count;

protected:
  vtkExtentModifier();
  ~vtkExtentModifier();

  virtual int RequestData(
			  vtkInformation* request,
			  vtkInformationVector** InputVector,
			  vtkInformationVector* outputVector);


  virtual int RequestUpdateExtent(
  			  vtkInformation* request,
  			  vtkInformationVector** InputVector,
  			  vtkInformationVector* outputVector);

  virtual int RequestInformation(
			  vtkInformation* request,
			  vtkInformationVector** InputVector,
			  vtkInformationVector* outputVector);
};

#if VTK_MAJOR_VERSION < 6
  vtkCxxRevisionMacro(vtkExtentModifier, "$Revision: 0.5$");
#endif
  vtkStandardNewMacro(vtkExtentModifier);

vtkExtentModifier::vtkExtentModifier(){this->count = 0;}
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
    output->SetExtent(input->GetExtent());
    outInfo->Set(output->DATA_EXTENT(), input->GetExtent(), 6);
    
    return 1;
  }

int vtkExtentModifier::RequestUpdateExtent(
			vtkInformation* request,
			vtkInformationVector** inputVector,
			vtkInformationVector* outputVector) {

  vtkInformation* outputInfo = outputVector->GetInformationObject(0);
  vtkInformation* inputInfo = inputVector[0]->GetInformationObject(0);

  int piece, npieces, ghostLevels, *extent;

  piece = outputInfo->Get(vtkStreamingDemandDrivenPipeline::UPDATE_PIECE_NUMBER());
  npieces = outputInfo->Get(vtkStreamingDemandDrivenPipeline::UPDATE_NUMBER_OF_PIECES());

  extent = outputInfo->Get(vtkStreamingDemandDrivenPipeline::UPDATE_EXTENT());

  std::cout << "Running Update Extent: "<<piece << " of " << npieces <<"\n";
  std::cout << " Extent : [" << extent[0];
  for (int i=1; i<6; ++i) {
    std::cout<<","<<extent[i];
  }
  std::cout << "]\n";

}

int vtkExtentModifier::RequestInformation(
			vtkInformation* request,
			vtkInformationVector** inputVector,
			vtkInformationVector* outputVector) {

  vtkInformation* outputInfo = outputVector->GetInformationObject(0);
  vtkInformation* inputInfo = inputVector[0]->GetInformationObject(0);

  int extent[6] = {0,30,0,10,0,0};

  outputInfo->Set(vtkStreamingDemandDrivenPipeline::WHOLE_EXTENT(), extent, 6);
  outputInfo->Set(vtkDataObject::DATA_EXTENT(), vtkRectilinearGrid::SafeDownCast(this->GetInput())->GetExtent(), 6);

  std::cout << ++this->count;

  return 1;

}

class myWriter : public vtkXMLPRectilinearGridWriter {
public:
  static myWriter* New();
#if VTK_MAJOR_VERSION < 6
  vtkTypeRevisionMacro(myWriter, vtkXMLPRectilinearGridWriter);
#else
 vtkTypeMacro(myWriter, vtkXMLPRectilinearGridWriter);
#endif

  vtkXMLStructuredDataWriter* CreateStructuredPieceWriter();
  

protected:
  myWriter();
  ~myWriter();
};

#if VTK_MAJOR_VERSION < 6
vtkCxxRevisionMacro(myWriter, "$Revision: 0.5$");
#endif
vtkStandardNewMacro(myWriter);

myWriter::myWriter(){}
myWriter::~myWriter(){}
vtkXMLStructuredDataWriter* myWriter::CreateStructuredPieceWriter(){

  vtkXMLRectilinearGridWriter* pWriter=vtkXMLRectilinearGridWriter::New();
  std::cout << "testing!";
  pWriter->SetInputConnection(this->GetInputConnection(0,0));
  std::cout << "hello boys!";
  return pWriter;

};

extern "C" {

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

    vtkExtentModifier *em;
    if (npieces > 1) {
      em = vtkExtentModifier::New();
      em->SetInput(grid);
      myWriter* writer = myWriter::New();
      writer->SetInput(em->GetOutput());
      writer->SetNumberOfPieces(npieces);
      writer->SetStartPiece(piece);
      writer->SetEndPiece(piece);
      writer->SetWriteSummaryFile(piece == 0);
      writer->SetFileName(name);
      writer->Write();
      writer->Delete();
      em->Delete();
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
