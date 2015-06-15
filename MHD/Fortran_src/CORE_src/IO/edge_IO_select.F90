!edge_IO_select.F90
!      module edge_IO_select
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine sel_output_edge_geometries(my_rank)
!      subroutine sel_output_edge_geometries_sph(my_rank)
!      subroutine sel_output_edge_geometries_cyl(my_rank)
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
