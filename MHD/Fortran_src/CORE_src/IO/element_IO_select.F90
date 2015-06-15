!element_IO_select.F90
!      module element_IO_select
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine sel_output_element_file(my_rank)
!      subroutine sel_output_element_sph_file(my_rank)
!      subroutine sel_output_element_cyl_file(my_rank)
!
      module element_IO_select
!
      use m_precision
!
      use m_read_mesh_data
      use m_file_format_switch
      use element_file_IO
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine sel_output_element_file(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_element_fname(my_rank)
      call output_element_file(my_rank)
!
      end subroutine sel_output_element_file
!
!------------------------------------------------------------------
!
      subroutine sel_output_element_sph_file(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_element_fname(my_rank)
      call output_element_sph_file(my_rank)
!
      end subroutine sel_output_element_sph_file
!
!------------------------------------------------------------------
!
      subroutine sel_output_element_cyl_file(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call set_element_fname(my_rank)
      call output_element_cyl_file(my_rank)
!
      end subroutine sel_output_element_cyl_file
!
!------------------------------------------------------------------
!
      end module element_IO_select
