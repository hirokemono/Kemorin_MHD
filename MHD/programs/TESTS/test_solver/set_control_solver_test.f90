!
!      module set_control_solver_test
!
!     Written by H. Matsui on July, 2006
!
!!      subroutine set_ctl_params_4_solver_test                         &
!!     &         (mat_crs, CG_param, DJDS_param)
!
      module set_control_solver_test
!
      use m_precision
      use t_crs_matrix
      use t_iccg_parameter
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_ctl_params_4_solver_test                           &
     &         (mat_crs, CG_param, DJDS_param)
!
      use calypso_mpi
      use m_machine_parameter
      use m_ctl_data_solver_test
      use crs_matrix_io
      use skip_comment_f
!
      use set_parallel_file_name
!
      type(CRS_matrix), intent(inout) :: mat_crs
      type(CG_poarameter), intent(inout) :: CG_param
      type(DJDS_poarameter), intent(inout) :: DJDS_param
!
!
      if (matrix_head_ctl%iflag .ne. 0) then
        matrix_file_head = matrix_head_ctl%charavalue
      else
        matrix_file_head = "matIN"
      end if
      matrix_file_name = add_int_suffix(my_rank, matrix_file_head)
      write(*,*) 'matrix data file: ', matrix_file_name
!
      if (solution_head_ctl%iflag .ne. 0) then
        solution_file_head = solution_head_ctl%charavalue
      else
        solution_file_head = "matIN"
      end if
      solution_file_name = add_int_suffix(my_rank, solution_file_head)
      write(*,*) 'solution data file: ', solution_file_name
!
      if (ip_smp_p_ctl%iflag .ne. 0) then
        np_smp = ip_smp_p_ctl%intvalue
      else
        np_smp = 1
      end if
!
!     set solver information
!
      if(solver_type_ctl%iflag .gt. 0) then
        mat_crs%SOLVER_crs =  solver_type_ctl%charavalue
      else
        mat_crs%SOLVER_crs = 'block33'
      end if
!
      call set_control_4_CG_solver(CG_test_ctl, CG_param)
      call set_control_4_DJDS_solver(CG_test_ctl%DJDS_ctl, DJDS_param)
      call copy_from_iccg_parameter(CG_param, mat_crs)
!
      if (iflag_debug .eq. 1) then
        write(*,*) 'np_smp       ', np_smp
        write(*,*) 'SOLVER_crs   ', mat_crs%SOLVER_crs
        write(*,*) 'METHOD_crs   ', mat_crs%METHOD_crs
        write(*,*) 'PRECOND_crs  ', mat_crs%PRECOND_crs
        write(*,*) 'INTARRAY_crs (iteration)', mat_crs%INTARRAY_crs(1)
        write(*,*) 'REALARRAY_crs (eps) ',                              &
     &            mat_crs%REALARRAY_crs(1)
        write(*,*) 'REALARRAY_crs (sigma_diag) ',                       &
     &            mat_crs%REALARRAY_crs(2)
        write(*,*) 'REALARRAY_crs (sigma) ',                            &
     &            mat_crs%REALARRAY_crs(3)
      end if
!
      end subroutine set_ctl_params_4_solver_test
!
!   --------------------------------------------------------------------
!
      end module set_control_solver_test
