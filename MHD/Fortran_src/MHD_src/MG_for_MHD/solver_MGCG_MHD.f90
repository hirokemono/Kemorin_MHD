!>@file   solver_MGCG_MHD.f90
!!@brief  module solver_MGCG_MHD
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2008
!!@date Modified in Nov., 2013
!
!>@brief  Wrapper for linear solvers for MHD dynmamo
!!
!!@verbatim
!!      subroutine solver_MGCG_velo
!!      subroutine solver_MGCG_press
!!      subroutine solver_MGCG_magne
!!      subroutine solver_MGCG_magne_p
!!      subroutine solver_MGCG_temp
!!      subroutine solver_MGCG_d_scalar
!!@endverbatim
!
      module   solver_MGCG_MHD
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
      use m_array_for_send_recv
      use m_iccg_parameter
      use m_ctl_parameter_Multigrid
      use m_geometry_parameter
      use m_work_time
!
      use m_type_AMG_data
      use m_type_AMG_data_4_MHD
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_MGCG_MHD
!
      use m_control_parameter
      use solver_DJDS11_struct
      use solver_DJDS33_struct
      use solver_VMGCG11_DJDS_SMP
      use solver_VMGCG33_DJDS_SMP
!
      integer(kind = kint) :: ierr
!
!
      method = method_4_solver
      if ( ((method(1:1).eq.'M').or.(method(1:1).eq.'m')) .and.         &
     &     ((method(2:2).eq.'G').or.(method(2:2).eq.'g')) .and.         &
     &     ((method(3:3).eq.'C').or.(method(3:3).eq.'c')) .and.         &
     &     ((method(4:4).eq.'G').or.(method(4:4).eq.'g')) ) then
        call init_VMGCG11_DJDS_SMP(numnod, np_smp,                      &
            precond_4_solver,  METHOD_MG, PRECOND_MG)
      else
        call init_DJDS11_struct(numnod, np_smp, method,                 &
            precond_4_solver, ierr)
      end if
!
!
      if(     iflag_t_evo_4_velo .ge.   id_Crank_nicolson               &
     &   .or. iflag_t_evo_4_vect_p .ge. id_Crank_nicolson               &
     &   .or. iflag_t_evo_4_magne .ge.  id_Crank_nicolson) then
        method = method_4_velo
        if ( ((method(1:1).eq.'M').or.(method(1:1).eq.'m')) .and.       &
     &       ((method(2:2).eq.'G').or.(method(2:2).eq.'g')) .and.       &
     &       ((method(3:3).eq.'C').or.(method(3:3).eq.'c')) .and.       &
     &       ((method(4:4).eq.'G').or.(method(4:4).eq.'g')) ) then
          call init_VMGCG33_DJDS_SMP(numnod, np_smp,                    &
              precond_4_crank,  METHOD_MG, PRECOND_MG)
        else
          call init33_DJDS_struct(numnod, np_smp, method,               &
              precond_4_crank, ierr)
        end if
      end if
!
      end subroutine init_MGCG_MHD
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine solver_MGCG_velo
!
      use m_solver_djds_MHD
      use m_velo_matrix
      use solver_DJDS33_struct
      use solver_VMGCG33_DJDS_SMP
!
      integer(kind = kint) :: ierr
!
!
      method = method_4_velo
      if (iflag_debug.eq.1) then
        write(*,*) 'method for velocity: ', trim(method_4_velo)
        write(*,*) 'smp: ', np_smp
      end if
! 
      call start_eleps_time(5)
      ierr = i_debug
!
      if ( ((method(1:1).eq.'M').or.(method(1:1).eq.'m')) .and.         &
     &     ((method(2:2).eq.'G').or.(method(2:2).eq.'g')) .and.         &
     &     ((method(3:3).eq.'C').or.(method(3:3).eq.'c')) .and.         &
     &     ((method(4:4).eq.'G').or.(method(4:4).eq.'g')) ) then
        call VMGCG33_DJDS_SMP(num_MG_level, MG_comm_fl,                 &
     &      MG_itp, MG_djds_tbl_fl, MG_mat_velo, MG_vector, np_smp,     &
     &      numnod, b_vec(1), x_vec(1), itr, itr_MG_mid, itr_MG_lowest, &
     &      eps_4_velo_crank, EPS_MG,                                   &
     &      precond_4_crank, METHOD_MG, PRECOND_MG, ierr)
      else
        call solve33_DJDS_struct(np_smp, DJDS_comm_fl, DJDS_fluid,      &
     &      Vmat_DJDS, numnod, b_vec(1), x_vec(1),                      &
     &      method_4_velo, precond_4_crank, ierr,                       &
     &      eps_4_velo_crank, itr, itr_res)
      end if
!
      call end_eleps_time(5)
      if (my_rank .eq. 0 ) then
        write(12,*) ' iteration_4_velocity:', itr_res
      end if
!
      end subroutine solver_MGCG_velo
!
! ----------------------------------------------------------------------
!
      subroutine solver_MGCG_press
!
      use m_solver_djds_MHD
      use m_velo_matrix
      use solver_DJDS11_struct
      use solver_VMGCG11_DJDS_SMP
!
      integer(kind = kint) :: ierr
!
!
       method = method_4_solver
      if (iflag_debug.eq.1) then
        write(*,*) 'method for pressure: ', trim(method_4_solver)
      end if
!
      call start_eleps_time(5)
      ierr = i_debug
!
      if ( ((method(1:1).eq.'M').or.(method(1:1).eq.'m')) .and.         &
     &     ((method(2:2).eq.'G').or.(method(2:2).eq.'g')) .and.         &
     &     ((method(3:3).eq.'C').or.(method(3:3).eq.'c')) .and.         &
     &     ((method(4:4).eq.'G').or.(method(4:4).eq.'g')) ) then
        call VMGCG11_DJDS_SMP(num_MG_level, MG_comm_fl,                 &
     &      MG_itp, MG_djds_tbl_fll, MG_mat_press, MG_vector, np_smp,   &
     &      numnod, b_vec(1), x_vec(1), itr, itr_MG_mid, itr_MG_lowest, &
     &      eps, EPS_MG, precond_4_solver, METHOD_MG, PRECOND_MG, ierr)
      else
        call solve_DJDS11_struct(np_smp, DJDS_comm_fl, DJDS_fl_l,       &
     &      Pmat_DJDS, numnod, b_vec(1), x_vec(1),                      &
     &      method_4_solver, precond_4_solver, ierr, eps, itr, itr_res)
      end if
!
      call end_eleps_time(5)
       if (my_rank .eq. 0 ) then
         write(12,*) ' iteration_4_pressure:', itr_res
       end if
!
      end subroutine solver_MGCG_press
!
! ----------------------------------------------------------------------
!
      subroutine solver_MGCG_magne
!
      use m_nod_comm_table
      use m_solver_djds_MHD
      use m_magne_matrix
      use solver_DJDS33_struct
      use solver_VMGCG33_DJDS_SMP
!
      integer(kind = kint) :: ierr
!
!
       method = method_4_velo
       if (iflag_debug.eq.1) then
         write(*,*) 'method for magne: ', trim(method_4_velo)
       end if
! 
      call start_eleps_time(5)
      ierr = i_debug
!
      if ( ((method(1:1).eq.'M').or.(method(1:1).eq.'m')) .and.         &
     &     ((method(2:2).eq.'G').or.(method(2:2).eq.'g')) .and.         &
     &     ((method(3:3).eq.'C').or.(method(3:3).eq.'c')) .and.         &
     &     ((method(4:4).eq.'G').or.(method(4:4).eq.'g')) ) then
        call VMGCG33_DJDS_SMP(num_MG_level, MG_comm,                    &
     &      MG_itp, MG_djds_tbl, MG_mat_magne, MG_vector, np_smp,       &
     &      numnod, b_vec(1), x_vec(1), itr, itr_MG_mid, itr_MG_lowest, &
     &      eps_4_magne_crank, EPS_MG,                                  &
     &      precond_4_crank, METHOD_MG, PRECOND_MG, ierr)
      else
        call solve33_DJDS_struct(np_smp, DJDS_comm_etr, DJDS_entire,    &
     &      Bmat_DJDS, numnod, b_vec(1), x_vec(1),                      &
     &      method_4_velo, precond_4_crank, ierr,                       &
     &      eps_4_magne_crank, itr, itr_res)
      end if
!
      call end_eleps_time(5)
      if (my_rank .eq. 0 ) then
        write(12,*) ' iteration_4_magnetic:', itr_res
      end if
!
      end subroutine solver_MGCG_magne
!
! ----------------------------------------------------------------------
!
      subroutine solver_MGCG_magne_p
!
      use m_nod_comm_table
      use m_solver_djds_MHD
      use m_magne_matrix
      use solver_DJDS11_struct
      use solver_VMGCG11_DJDS_SMP
!
      integer(kind = kint) :: ierr
!
!
      method = method_4_solver
      if (iflag_debug.eq.1) then
        write(*,*) 'method for scalar potential: ',                     &
     &              trim(method_4_solver)
      end if
!
      call start_eleps_time(5)
      ierr = i_debug
!
      if ( ((method(1:1).eq.'M').or.(method(1:1).eq.'m')) .and.         &
     &     ((method(2:2).eq.'G').or.(method(2:2).eq.'g')) .and.         &
     &     ((method(3:3).eq.'C').or.(method(3:3).eq.'c')) .and.         &
     &     ((method(4:4).eq.'G').or.(method(4:4).eq.'g')) ) then
        call VMGCG11_DJDS_SMP(num_MG_level, MG_comm,                    &
     &      MG_itp, MG_djds_tbl_l, MG_mat_magp, MG_vector, np_smp,      &
     &      numnod, b_vec(1), x_vec(1), itr, itr_MG_mid, itr_MG_lowest, &
     &      eps, EPS_MG, precond_4_solver, METHOD_MG, PRECOND_MG, ierr)
      else
        call solve_DJDS11_struct(np_smp, DJDS_comm_etr, DJDS_linear,    &
     &      Fmat_DJDS, numnod, b_vec(1), x_vec(1),                      &
     &      method_4_solver, precond_4_solver, ierr, eps, itr, itr_res)
      end if
!
      call end_eleps_time(5)
      if (my_rank .eq. 0 ) then
        write(12,*) ' iteration_4_mag_potential:', itr_res
      end if
!
      end subroutine solver_MGCG_magne_p
!
! ----------------------------------------------------------------------
!
      subroutine solver_MGCG_temp
!
      use m_solver_djds_MHD
      use m_temp_matrix
      use solver_DJDS11_struct
      use solver_VMGCG11_DJDS_SMP
!      use solver_CG
!
      integer(kind = kint) :: ierr
!
!
      method = method_4_solver
      if (iflag_debug.eq.1) then
        write(*,*) 'method for temp: ', trim(method_4_solver)
      end if
!
      call start_eleps_time(5)
      ierr = i_debug
!
!      call CG                                                          &
!     &   ( internal_node, numnod, ntot_l, ntot_u,                      &
!     &     d_crs, al_crs, istack_l_crs, item_l_crs, au_crs,            &
!     &     istack_u_crs, item_u_crs, b_vec(1), x_vec(1),               &
!     &     precond_4_solver,1.0d0, 1.0d0, eps_4_temp_crank,            &
!     &     itr, ierr, my_rank,                                         &
!     &     DJDS_comm_fl%num_neib, DJDS_comm_fl%id_neib,                &
!     &     DJDS_comm_fl%istack_import, DJDS_comm_fl%item_import,       &
!     &     DJDS_comm_fl%istack_export, DJDS_comm_fl%item_export, 1)
!       call deallocate_check_djds_array
!
      if ( ((method(1:1).eq.'M').or.(method(1:1).eq.'m')) .and.         &
     &     ((method(2:2).eq.'G').or.(method(2:2).eq.'g')) .and.         &
     &     ((method(3:3).eq.'C').or.(method(3:3).eq.'c')) .and.         &
     &     ((method(4:4).eq.'G').or.(method(4:4).eq.'g')) ) then
        call VMGCG11_DJDS_SMP(num_MG_level, MG_comm_fl,                 &
     &      MG_itp, MG_djds_tbl_fl, MG_mat_temp, MG_vector, np_smp,     &
     &      numnod, b_vec(1), x_vec(1), itr, itr_MG_mid, itr_MG_lowest, &
     &      eps_4_temp_crank, EPS_MG, precond_4_solver,                 &
     &      METHOD_MG, PRECOND_MG, ierr)
      else
        call solve_DJDS11_struct(np_smp, DJDS_comm_fl, DJDS_fluid,      &
     &      Tmat_DJDS, numnod, b_vec(1), x_vec(1),                      &
     &      method_4_solver, precond_4_solver, ierr, eps_4_temp_crank,  &
     &      itr, itr_res)
      end if
!
      call end_eleps_time(5)
      if (my_rank .eq. 0 ) then
        write(12,*) ' iteration_4_temp.:', itr_res
      end if
!
      end subroutine solver_MGCG_temp
!
! ----------------------------------------------------------------------
!
      subroutine solver_MGCG_d_scalar
!
      use m_solver_djds_MHD
      use m_light_element_matrix
      use solver_DJDS11_struct
      use solver_VMGCG11_DJDS_SMP
!
      integer(kind = kint) :: ierr
!
!
      method = method_4_solver
      if (iflag_debug.eq.1) then
        write(*,*) 'method for scalar: ', trim(method_4_solver)
      end if
!
      call start_eleps_time(5)
      ierr = i_debug
!
!       write(50+my_rank,*) 'inod, b_vec(inod), x_vec(inod)'
!       do inod = 1, numnod
!        write(50+my_rank,*) inod, b_vec(inod), x_vec(inod)
!       end do
!
      if ( ((method(1:1).eq.'M').or.(method(1:1).eq.'m')) .and.         &
     &     ((method(2:2).eq.'G').or.(method(2:2).eq.'g')) .and.         &
     &     ((method(3:3).eq.'C').or.(method(3:3).eq.'c')) .and.         &
     &     ((method(4:4).eq.'G').or.(method(4:4).eq.'g')) ) then
        call VMGCG11_DJDS_SMP(num_MG_level, MG_comm_fl,                 &
     &      MG_itp, MG_djds_tbl_fl, MG_mat_d_scalar, MG_vector, np_smp, &
     &      numnod, b_vec(1), x_vec(1), itr, itr_MG_mid, itr_MG_lowest, &
     &      eps_4_d_scalar_crank, EPS_MG, precond_4_solver,             &
     &      METHOD_MG, PRECOND_MG, ierr)
      else
        call solve_DJDS11_struct(np_smp, DJDS_comm_fl, DJDS_fluid,      &
     &      Cmat_DJDS, numnod, b_vec(1), x_vec(1),                      &
     &      method_4_solver, precond_4_solver, ierr,                    &
     &      eps_4_d_scalar_crank, itr, itr_res)
      end if
!
      call end_eleps_time(5)
      if (my_rank .eq. 0 ) then
        write(12,*) ' iteration for composition:', itr_res
      end if
!
      end subroutine solver_MGCG_d_scalar
!
! ----------------------------------------------------------------------
!
      end module solver_MGCG_MHD
