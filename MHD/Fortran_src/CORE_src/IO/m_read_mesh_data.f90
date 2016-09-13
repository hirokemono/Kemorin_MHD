!>@file   m_read_mesh_data.f90
!!@brief  module m_read_mesh_data
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui
!
!>@brief Data arry for mesh_data_IO
!!
!!@verbatim
!!@endverbatim
!
      module m_read_mesh_data
!
      use m_precision
!
      implicit  none
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
      integer(kind = kint), parameter ::  input_file_code = 14
!   i/o code for ucd data output file
!
      end module m_read_mesh_data
