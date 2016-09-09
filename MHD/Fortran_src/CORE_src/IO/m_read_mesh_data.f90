!>@file   m_read_mesh_data.f90
!!@brief  module m_read_mesh_data
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui
!
!>@brief Data arry for mesh_data_IO
!!
!!@verbatim
!!      subroutine allocate_ele_info_dummy
!!      subroutine allocate_connect_dummy
!!
!!      subroutine deallocate_mesh_arrays
!!
!!      subroutine deallocate_ele_info_dummy
!!
!!      subroutine allocate_ele_scalar_IO
!!      subroutine deallocate_ele_scalar_IO
!!@endverbatim
!
      module m_read_mesh_data
!
      use m_precision
      use t_geometry_data
!
      implicit  none
!
!>  structure for node data IO (position)
      type(node_data), save ::    nod_IO
!
!>  structure for element data IO (connectivity)
      type(element_data), save :: ele_IO
!
!
      real(kind=kreal),   allocatable :: ele_vector_IO(:,:)
      real(kind=kreal),   allocatable :: ele_scalar_IO(:)
!
      integer(kind = kint) :: nsf_4_ele_IO
      integer(kind = kint) :: nsurf_in_ele_IO = 6
      integer(kind = kint), allocatable  :: isf_4_ele_IO(:,:)
!
      integer(kind = kint) :: ned_4_ele_IO
      integer(kind = kint) :: nedge_in_ele_IO = 12
      integer(kind = kint), allocatable  :: iedge_4_ele_IO(:,:)
!
!
      integer(kind = kint)  :: iflag_mesh_file_fmt = 0
!
      character(len=kchara), parameter:: def_mesh_file_head = 'mesh/in'
      character(len=kchara), parameter                                  &
                  :: mesh_ele_def_head = 'mesh/element'
      character(len=kchara), parameter                                  &
     &            :: mesh_def_surf_head = 'mesh/surface'
      character(len=kchara), parameter                                  &
     &            :: mesh_def_edge_head = 'mesh/edge'
!
      character(len=kchara) :: mesh_file_head =      def_mesh_file_head
      character(len=kchara) :: mesh_ele_file_head =  mesh_ele_def_head
      character(len=kchara) :: mesh_surf_file_head = mesh_def_surf_head
      character(len=kchara) :: mesh_edge_file_head = mesh_def_edge_head
!
!
      character(len=kchara) :: mesh_file_name
!
!
!   mesh file name
      integer(kind=kint ) ::  input_file_code = 14
!   i/o code for ucd data output file
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine allocate_surface_connect_IO
!
      allocate( isf_4_ele_IO(nsf_4_ele_IO,nsurf_in_ele_IO) )
      isf_4_ele_IO = 0
!
      end subroutine allocate_surface_connect_IO
!
!------------------------------------------------------------------
!
      subroutine allocate_edge_connect_IO
!
      allocate( iedge_4_ele_IO(ned_4_ele_IO,nedge_in_ele_IO) )
      iedge_4_ele_IO = 0
!
      end subroutine allocate_edge_connect_IO
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine deallocate_surface_connect_IO
!
      deallocate( isf_4_ele_IO )
!
      end subroutine deallocate_surface_connect_IO
!
!------------------------------------------------------------------
!
      subroutine deallocate_edge_connect_IO
!
      deallocate( iedge_4_ele_IO )
!
      end subroutine deallocate_edge_connect_IO
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine allocate_ele_vector_IO
!
      allocate( ele_vector_IO(nod_IO%numnod,3) )
      ele_vector_IO = 0.0d0
!
      end subroutine allocate_ele_vector_IO
!
!------------------------------------------------------------------
!
      subroutine deallocate_ele_vector_IO
!
      deallocate( ele_vector_IO )
!
      end subroutine deallocate_ele_vector_IO
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine allocate_ele_scalar_IO
!
      allocate( ele_scalar_IO(nod_IO%numnod) )
      ele_scalar_IO = 0.0d0
!
      end subroutine allocate_ele_scalar_IO
!
!------------------------------------------------------------------
!
      subroutine deallocate_ele_scalar_IO
!
      deallocate( ele_scalar_IO )
!
      end subroutine deallocate_ele_scalar_IO
!
!------------------------------------------------------------------
!
      end module m_read_mesh_data
