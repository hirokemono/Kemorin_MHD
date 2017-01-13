!
!      module set_control_solver_test
!
!     Written by H. Matsui on July, 2006
!
!      subroutine set_ctl_params_4_solver_test
!
      module set_control_solver_test
!
      use m_precision
      use t_crs_matrix
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_ctl_params_4_solver_test(mat_crs)
!
      use calypso_mpi
      use m_machine_parameter
      use m_type_AMG_mesh
      use m_iccg_parameter
      use m_ctl_parameter_Multigrid
      use m_ctl_data_solver_test
      use crs_matrix_io
      use skip_comment_f
!
      use set_parallel_file_name
!
      type(CRS_matrix), intent(inout) :: mat_crs
!
!
      if (matrix_head_ctl%iflag .ne. 0) then
        matrix_file_head = matrix_head_ctl%charavalue
      else
        matrix_file_head = "matIN"
      end if
      call add_int_suffix(my_rank, matrix_file_head, matrix_file_name)
      write(*,*) 'matrix data file: ', matrix_file_name
!
      if (solution_head_ctl%iflag .ne. 0) then
        solution_file_head = solution_head_ctl%charavalue
      else
        solution_file_head = "matIN"
      end if
      call add_int_suffix(my_rank, solution_file_head,                  &
     &    solution_file_name)
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
      if(CG_test_ctl%precond_ctl%iflag .gt. 0) then
        precond = CG_test_ctl%precond_ctl%charavalue
      end if
      if(CG_test_ctl%method_ctl%iflag .gt. 0)  then
        method =  CG_test_ctl%method_ctl%charavalue
      end if
      if(CG_test_ctl%eps_ctl%iflag .gt. 0)   then
        eps = CG_test_ctl%eps_ctl%realvalue
      end if
      if(CG_test_ctl%itr_ctl%iflag .gt. 0)   then
        itr = CG_test_ctl%itr_ctl%intvalue
      end if
      if(CG_test_ctl%sigma_ctl%iflag .gt. 0) then
        sigma = CG_test_ctl%sigma_ctl%realvalue
      end if
      if(CG_test_ctl%sigma_diag_ctl%iflag .gt. 0) then
        sigma_diag =  CG_test_ctl%sigma_diag_ctl%realvalue
      end if
!
      mat_crs%METHOD_crs =       method
      mat_crs%PRECOND_crs =      precond
      mat_crs%INTARRAY_crs(1) =  itr
      mat_crs%REALARRAY_crs(1) = eps
      mat_crs%REALARRAY_crs(2) = sigma_diag
      mat_crs%REALARRAY_crs(3) = sigma
!
      call set_control_4_DJDS_solver(CG_test_ctl%DJDS_ctl)
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
!
        write(*,*) 'iflag_ordering', iflag_ordering
        write(*,*) 'min_color', min_color
        write(*,*) 'mc_color', mc_color
      end if
!
      if(cmp_no_case(mat_crs%METHOD_crs, 'MGCG')) then
        call set_ctl_data_4_Multigrid(CG_test_ctl%MG_ctl)
      end if
!
      end subroutine set_ctl_params_4_solver_test
!
!   --------------------------------------------------------------------
!
      end module set_control_solver_test
