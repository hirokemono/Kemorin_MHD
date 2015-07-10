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
!------------------------------------------------------------------
!
       contains
!
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
