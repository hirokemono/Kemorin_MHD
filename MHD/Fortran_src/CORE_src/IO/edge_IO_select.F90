!>@file  edge_IO_select.f90
!!      module edge_IO_select
!!
!!@author  H. Matsui
!!@date Programmed in Aug., 2006
!
!>@brief Edge file IO selector
!!
!!@verbatim
!!      subroutine sel_output_edge_geometries(my_rank)
!!      subroutine sel_output_edge_geometries_sph(my_rank)
!!      subroutine sel_output_edge_geometries_cyl(my_rank)
!!@endverbatim
!
!
      module edge_IO_select
!
      use m_precision
!
      use m_file_format_switch
      use edge_file_IO
!
      implicit none
!
      private :: set_edge_fname
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine set_edge_fname(my_rank)
!
      use m_file_format_switch
      use set_mesh_file_names
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara) :: fname_tmp
!
!
      call set_mesh_file_name(mesh_edge_file_head, id_ascii_file_fmt,   &
     &    my_rank, mesh_file_name)
!
      end subroutine set_edge_fname
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_output_edge_geometries(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_edge_fname(my_rank)
      call output_edge_geometries
!
      end subroutine sel_output_edge_geometries
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_output_edge_geometries_sph(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_edge_fname(my_rank)
      call output_edge_geometries_sph
!
      end subroutine sel_output_edge_geometries_sph
!
!------------------------------------------------------------------
!
      subroutine sel_output_edge_geometries_cyl(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_edge_fname(my_rank)
      call output_edge_geometries_cyl
!
      end subroutine sel_output_edge_geometries_cyl
!
!------------------------------------------------------------------
!
      end module edge_IO_select
