!>@file  element_file_IO.f90
!!      module element_file_IO
!!
!!@author  H. Matsui
!!@date Programmed in Aug., 2006
!
!>@brief File IO for element communication table
!!
!!@verbatim
!!      subroutine set_element_fname(my_rank)
!!
!!      subroutine output_element_file(my_rank)
!!      subroutine output_element_sph_file(my_rank)
!!      subroutine output_element_cyl_file(my_rank)
!!@endverbatim
!!
!!@param my_rank  MPI rank
!
      module element_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use m_read_mesh_data
      use set_parallel_file_name
      use element_data_IO
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine set_element_fname(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara) :: fname_tmp
!
!
      if(iflag_mesh_file_ext.gt.0) then
        call add_int_suffix(my_rank, mesh_ele_file_head, fname_tmp)
        call add_gfm_extension(fname_tmp, mesh_file_name)
      else
        call add_int_suffix(my_rank, mesh_ele_file_head,                &
     &      mesh_file_name)
      end if
!
      end subroutine set_element_fname
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine output_element_file(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write ascii element comm file: ', trim(mesh_file_name)
!
      open(input_file_code, file = mesh_file_name, form = 'formatted')
      call write_element_comm_table
      call write_element_geometry
      close(input_file_code)
!
      end subroutine output_element_file
!
!------------------------------------------------------------------
!
      subroutine output_element_sph_file(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write ascii element comm file: ', trim(mesh_file_name)
!
      open(input_file_code, file = mesh_file_name, form = 'formatted')
      call write_element_comm_table
      call write_element_geometry_sph
      close(input_file_code)
!
      end subroutine output_element_sph_file
!
!------------------------------------------------------------------
!
      subroutine output_element_cyl_file(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write ascii element comm file: ', trim(mesh_file_name)
!
      open(input_file_code, file = mesh_file_name, form = 'formatted')
      call write_element_comm_table
      call write_element_geometry_cyl
      close(input_file_code)
!
      end subroutine output_element_cyl_file
!
!------------------------------------------------------------------
!
      end module element_file_IO
