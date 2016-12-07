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
      use t_file_IO_parameter
!
      implicit  none
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
!>      Structure for field data IO paramters
      type(field_IO_params), save ::  mesh1_file
!mesh1_file%iflag_format
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine copy_mesh_format_and_prefix(prefix, i_format)
!
      character(len=kchara), intent(in) :: prefix
      integer(kind = kint), intent(in)  :: i_format
!
!
      mesh_file_head =  prefix
!      mesh1_file%file_prefix =  prefix
      mesh1_file%iflag_format = i_format
      mesh1_file%iflag_IO =     i_format
!
      end subroutine copy_mesh_format_and_prefix
!
!  ---------------------------------------------------------------------
!
      end module m_read_mesh_data
