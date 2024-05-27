!>@file   collect_fline_data.f90
!!@brief  module collect_fline_data
!!
!!@author  H. Matsui
!!@date Programmed on Aug., 2011
!
!> @brief MPI communication To collect field line data
!!
!!@verbatim
!!      subroutine copy_local_fieldline_to_IO(color_name_gl,            &
!!     &                                      fline_lc, ucd)
!!        character(len = kchara), intent(in) :: color_name_gl
!!        type(local_fieldline), intent(in) :: fline_lc
!!        type(ucd_data), intent(inout) :: ucd
!!
!!      subroutine s_collect_fline_data(istep_fline, fln_prm,           &
!!     &          fline_lc, fline_gl)
!!        type(fieldline_paramter), intent(in) :: fln_prm
!!        type(local_fieldline), intent(in) :: fline_lc
!!        type(global_fieldline_data), intent(inout) :: fline_gl
!!@endverbatim
!
      module collect_fline_data
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_geometry_constants
      use t_global_fieldline
      use t_local_fline
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine copy_local_fieldline_to_IO(color_name_gl,              &
     &                                      fline_lc, ucd)
!
      use t_ucd_data
      use const_global_element_ids
!
      character(len = kchara), intent(in) :: color_name_gl
      type(local_fieldline), intent(in) :: fline_lc
      type(ucd_data), intent(inout) :: ucd
!
      integer(kind = kint_gl) :: i, nd
!
!
      ucd%nnod = fline_lc%nnod_line_l
      ucd%nele = fline_lc%nele_line_l
      ucd%nnod_4_ele = num_linear_edge
!
      call alloc_merged_ucd_nod_stack(nprocs, ucd)
      call alloc_merged_ucd_ele_stack(nprocs, ucd)
      call count_number_of_node_stack(fline_lc%nnod_line_l,             &
     &                                ucd%istack_merged_nod)
      call count_number_of_node_stack(fline_lc%nele_line_l,             &
     &                                ucd%istack_merged_ele)
      write(*,*) 'ucd%istack_merged_nod', ucd%istack_merged_nod
      write(*,*) 'ucd%istack_merged_ele', ucd%istack_merged_ele
!
!$omp parallel workshare
      ucd%istack_merged_intnod(0:nprocs)                                &
     &                  = ucd%istack_merged_nod(0:nprocs)
!$omp end parallel workshare
!
      call allocate_ucd_node(ucd)
!$omp parallel do
      do i = 1, ucd%nnod
        ucd%inod_global(i) = i + ucd%istack_merged_nod(my_rank)
        ucd%xx(i,1) = fline_lc%xx_line_l(1,i)
        ucd%xx(i,2) = fline_lc%xx_line_l(2,i)
        ucd%xx(i,3) = fline_lc%xx_line_l(3,i)
      end do
!$omp end parallel do

      call allocate_ucd_ele(ucd)
!$omp parallel do
      do i = 1, ucd%nele
        ucd%iele_global(i) = i + ucd%istack_merged_ele(my_rank)
        ucd%ie(i,1) = fline_lc%iedge_line_l(1,i)                        &
     &               + ucd%istack_merged_nod(my_rank)
        ucd%ie(i,2) = fline_lc%iedge_line_l(2,i)                        &
     &               + ucd%istack_merged_nod(my_rank)
      end do
!$omp end parallel do
      
      ucd%num_field = 1
      call allocate_ucd_phys_name(ucd)
!$omp parallel workshare
      ucd%phys_name(1:ucd%num_field) = color_name_gl
      ucd%num_comp(1:ucd%num_field) =  1
!$omp end parallel workshare

      ucd%ntot_comp = sum(ucd%num_comp)
      call allocate_ucd_phys_data(ucd)
      do nd = 1, ucd%ntot_comp
!$omp parallel workshare
        ucd%d_ucd(1:ucd%nnod,nd) = fline_lc%col_line_l(1:ucd%nnod)
!$omp end parallel workshare
      end do
!
      end subroutine copy_local_fieldline_to_IO
!
!  ---------------------------------------------------------------------
!
      subroutine s_collect_fline_data(istep_fline, fln_prm,             &
     &          fline_lc, fline_gl)
!
      use t_control_params_4_fline
      use m_field_file_format
      use set_ucd_file_names
      use set_parallel_file_name
      use set_ucd_extensions
      use collect_fline_connectivity
      use collect_fline_position
!
      integer(kind = kint), intent(in) :: istep_fline
      type(fieldline_paramter), intent(in) :: fln_prm
      type(local_fieldline), intent(in) :: fline_lc
!
      type(global_fieldline_data), intent(inout) :: fline_gl
!
      character(len=kchara) :: file_name
!
!
      fline_gl%color_name_gl = fln_prm%name_color_output
      call collect_number_of_fline(fline_lc, fline_gl)
      write(*,*) 'fline_gl%istack_nod_line_gl', fline_gl%istack_nod_line_gl
      write(*,*) 'fline_gl%istack_ele_line_gl', fline_gl%istack_ele_line_gl
!
      if(fline_gl%ntot_nod_line_gl                                      &
     &        .gt. fline_gl%ntot_nod_line_gl_buf) then
        call raise_global_fline_data(fline_gl)
      end if
!
      if(fline_gl%ntot_ele_line_gl                                      &
     &       .gt. fline_gl%ntot_ele_line_gl_buf) then
        call raise_global_fline_connect(fline_gl)
      end if
!
      call collect_fline_connection(fline_lc, fline_gl)
      call s_collect_fline_position(fline_lc, fline_gl)
      call collect_fline_color(fline_lc, fline_gl)
!
!
      if(my_rank .eq. 0) then
        write(*,*) 'output format ', fln_prm%fline_file_IO%iflag_format
!!        if(fln_prm%fline_file_IO%iflag_format .eq. iflag_ucd) then
!          file_name = set_single_ucd_file_name(fln_prm%fline_file_IO%file_prefix,    &
!     &               fln_prm%fline_file_IO%iflag_format, istep_fline)
!          write(*,*) 'output ', trim(file_name)
!          open(id_fline_data_code, file=file_name)
!
!          call write_global_fline_ucd(id_fline_data_code, fline_gl)
!          close(id_fline_data_code)
!!        else if(fln_prm%fline_file_IO%iflag_format .eq. iflag_vtk)               &
!!     &        then
          write(file_name,'(a,a4)') trim(fln_prm%fline_file_IO%file_prefix), '.vtk'
          write(*,*) 'output ', trim(file_name)
          open(id_fline_data_code, file=file_name)
!
          call write_global_fline_vtk(id_fline_data_code, fline_gl)
          close(id_fline_data_code)
!        else
!          file_name = add_int_suffix(istep_fline, fln_prm%fline_file_IO%file_prefix)
!          file_name = add_dx_extension(file_name)
!          write(*,*) 'output ', trim(file_name)
!          open(id_fline_data_code, file=file_name)
!
!          call write_global_fline_dx(id_fline_data_code, fline_gl)
!          close(id_fline_data_code)
!        end if
      end if
!
      end subroutine s_collect_fline_data
!
!  ---------------------------------------------------------------------
!
      end module collect_fline_data
