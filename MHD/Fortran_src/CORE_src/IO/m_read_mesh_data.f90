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
      character(len=kchara), parameter                                  &
     &                   :: mesh_sph_def_head = 'mesh/node_sph'
      character(len=kchara), parameter                                  &
     &                   :: mesh_cyl_def_head = 'mesh/node_cyl'
!
!
      character(len=kchara) :: mesh_file_head =      def_mesh_file_head
!
      end module m_read_mesh_data
