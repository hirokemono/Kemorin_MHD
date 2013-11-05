!
!     module   solver_MGCG_MHD
!
!     Written by H. Matsui on Apr., 2008
!
!      subroutine init_MGCG_MHD
!
!      subroutine solver_MGCG_velo
!      subroutine solver_MGCG_press
!      subroutine solver_MGCG_magne
!      subroutine solver_MGCG_magne_p
!      subroutine solver_MGCG_temp
!      subroutine solver_MGCG_d_scalar
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
      use solver_DJDS
      use solver33_DJDS
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
        call init_solver_DJDS(numnod, np_smp, method,                   &
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
          call init_solver33_DJDS(numnod, np_smp, method,               &
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
      use m_comm_table_4_MHD
      use m_solver_djds_fluid
      use m_velo_matrix
      use solver33_DJDS
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
        call solve33_DJDS_kemo                                          &
     &     (internal_node, numnod, NLmax, NUmax,                        &
     &     itotal_fl_l, itotal_fl_u, NHYP, np_smp, inter_smp_stack,     &
     &     STACKmc, NLmaxHYP, NUmaxHYP, IVECT, NEWtoOLD,                &
     &     OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U, NEWtoOLD_DJDS_U, LtoU,     &
     &     Vmat_DJDS%D, b_vec(1), x_vec(1),                             &
     &     indexDJDS_L, indexDJDS_U, itemDJDS_L, itemDJDS_U,            &
     &     Vmat_DJDS%AL, Vmat_DJDS%AU,                                  &
     &     Vmat_DJDS%ALUG_L, Vmat_DJDS%ALUG_U, eps_4_velo_crank,        &
     &     itr, ierr, neigh_pe_num_fl, neigh_pe_data_fl,                &
     &     istack_import_fl, item_import_fl,                            &
     &     istack_export_fl, NOD_EXPORT_NEW_fl,                         &
     &     method_4_velo, precond_4_crank, itr_res)
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
      use m_comm_table_4_MHD
      use m_solver_djds_linear_fl
      use m_velo_matrix
      use solver_DJDS
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
        call solve_DJDS_kemo                                            &
     &     (internal_node, numnod, NLmax1, NUmax1,                      &
     &     itotal1_fl_l, itotal1_fl_u,  NHYP1, np_smp, inter_smp_stack, &
     &     STACKmc1, NLmaxHYP1, NUmaxHYP1, IVECT1, NEWtoOLD1,           &
     &     OLDtoNEW_DJDS1_L, OLDtoNEW_DJDS1_U, NEWtoOLD_DJDS1_U, LtoU1, &
     &     Pmat_DJDS%D, b_vec(1), x_vec(1),                             &
     &     indexDJDS1_L, indexDJDS1_U, itemDJDS1_L, itemDJDS1_U,        &
     &     Pmat_DJDS%AL, Pmat_DJDS%AU,                                  &
     &     Pmat_DJDS%ALUG_L, Pmat_DJDS%ALUG_U, eps, itr, ierr,          &
     &     neigh_pe_num_fl, neigh_pe_data_fl,                           &
     &     istack_import_fl, item_import_fl,                            &
     &     istack_export_fl, NOD_EXPORT_NEW_fl1,                        &
     &     method_4_solver, precond_4_solver, itr_res)
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
      use m_solver_djds
      use m_magne_matrix
      use solver33_DJDS
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
        call solve33_DJDS_kemo                                          &
     &    (internal_node, numnod, NLmax, NUmax, itotal_l, itotal_u,     &
     &     NHYP, np_smp, inter_smp_stack, STACKmc, NLmaxHYP, NUmaxHYP,  &
     &     IVECT, NEWtoOLD, OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U,           &
     &     NEWtoOLD_DJDS_U, LtoU, aiccg_magne(im_mag_d), b_vec(1),      &
     &     x_vec(1), indexDJDS_L, indexDJDS_U, itemDJDS_L, itemDJDS_U,  &
     &     aiccg_magne(im_mag_l), aiccg_magne(im_mag_u),                &
     &     ALUG_magne_L, ALUG_magne_U, eps_4_magne_crank, itr, ierr,    &
     &     num_neib, id_neib, istack_import, item_import,               &
     &     istack_export, NOD_EXPORT_NEW,                               &
     &     method_4_velo, precond_4_crank, itr_res)
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
      use m_solver_djds_linear
      use m_mag_potential_matrix
      use solver_DJDS
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
        call solve_DJDS_kemo                                            &
     &    (internal_node, numnod, NLmax1, NUmax1, itotal1_l, itotal1_u, &
     &     NHYP1, np_smp, inter_smp_stack, STACKmc1,                    &
     &     NLmaxHYP1, NUmaxHYP1, IVECT1, NEWtoOLD1,                     &
     &     OLDtoNEW_DJDS1_L, OLDtoNEW_DJDS1_U, NEWtoOLD_DJDS1_U,        &
     &     LtoU1, aiccg_mag_p(im_mp_d), b_vec(1), x_vec(1),             &
     &     indexDJDS1_L, indexDJDS1_U, itemDJDS1_L, itemDJDS1_U,        &
     &     aiccg_mag_p(im_mp_l), aiccg_mag_p(im_mp_u),                  &
     &     ALUG_mag_p_L, ALUG_mag_p_U,                                  &
     &     eps, itr, ierr, num_neib, id_neib,                           &
     &     istack_import, item_import,                                  &
     &     istack_export, NOD_EXPORT_NEW1,                              &
     &     method_4_solver, precond_4_solver, itr_res)
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
      use m_comm_table_4_MHD
      use m_solver_djds_fluid
      use m_temp_matrix
      use solver_DJDS
      use solver_VMGCG11_DJDS_SMP
!      use check_DJDS_ordering
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
!       call s_check_DJDS_ordering                                      &
!     &    (internal_node ,numnod, NLmax, NUmax,                        &
!     &     itotal_fl_l, itotal_fl_u, (NLmax*np_smp), (NUmax*np_smp),   &
!     &     NHYP, np_smp, inter_smp_stack, STACKmc, NLmaxHYP, NUmaxHYP, &
!     &     NEWtoOLD, OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U, NEWtoOLD_DJDS_U,&
!     &     LtoU, indexDJDS_L, indexDJDS_U, itemDJDS_L, itemDJDS_U,     &
!     &     my_rank)
!     
!       call reverse_DJDS_matrix                                        &
!     &    (internal_node, numnod, NLmax, NUmax,                        &
!     &     itotal_fl_l, itotal_fl_u, (NLmax*np_smp), (NUmax*np_smp),   &
!     &     NHYP, np_smp, inter_smp_stack, STACKmc, NLmaxHYP, NUmaxHYP, &
!     &     NEWtoOLD, OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U, NEWtoOLD_DJDS_U,&
!     &     LtoU, indexDJDS_L, indexDJDS_U, itemDJDS_L, itemDJDS_U,     &
!     &     Tmat_DJDS%D, Tmat_DJDS%AL, Tmat_DJDS%AU, my_rank)
!      call CG                                                          &
!     &   ( internal_node, numnod, ntot_l, ntot_u,                      &
!     &     d_crs, al_crs, istack_l_crs, item_l_crs, au_crs,            &
!     &     istack_u_crs, item_u_crs, b_vec(1), x_vec(1),               &
!     &     precond_4_solver,1.0d0, 1.0d0, eps_4_temp_crank,            &
!     &     itr, ierr, my_rank,                                         &
!     &     neigh_pe_num_fl, neigh_pe_data_fl,                          &
!     &     istack_import_fl, item_import_fl,                           &
!     &     istack_export_fl, item_export_fl, 1)
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
        call solve_DJDS_kemo                                            &
     &     (internal_node, numnod, NLmax, NUmax,                        &
     &      itotal_fl_l, itotal_fl_u, NHYP, np_smp, inter_smp_stack,    &
     &      STACKmc, NLmaxHYP, NUmaxHYP, IVECT, NEWtoOLD,               &
     &      OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U, NEWtoOLD_DJDS_U, LtoU,    &
     &      Tmat_DJDS%D, b_vec(1), x_vec(1),                            &
     &      indexDJDS_L, indexDJDS_U, itemDJDS_L, itemDJDS_U,           &
     &      Tmat_DJDS%AL, Tmat_DJDS%AU,                                 &
     &      Tmat_DJDS%ALUG_L, Tmat_DJDS%ALUG_U, eps_4_temp_crank,       &
     &      itr, ierr, neigh_pe_num_fl, neigh_pe_data_fl,               &
     &      istack_import_fl, item_import_fl,                           &
     &      istack_export_fl, NOD_EXPORT_NEW_fl,                        &
     &      method_4_solver, precond_4_solver, itr_res)
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
      use m_comm_table_4_MHD
      use m_solver_djds_fluid
      use m_light_element_matrix
      use solver_DJDS
      use solver_VMGCG11_DJDS_SMP
!      use check_DJDS_ordering
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
!       call s_check_DJDS_ordering(internal_node, numnod,               &
!     &      NLmax, NUmax, itotal_fl_l, itotal_fl_u,                    &
!     &      (NLmax*np_smp), (NUmax*np_smp), NHYP, np_smp,              &
!     &      inter_smp_stack, STACKmc, NLmaxHYP, NUmaxHYP, NEWtoOLD,    &
!     &      OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U, NEWtoOLD_DJDS_U,         &
!     &      LtoU, indexDJDS_L, indexDJDS_U, itemDJDS_L, itemDJDS_U,    &
!     &      my_rank)
!       call reverse_DJDS_matrix(internal_node, numnod,                 &
!     &      NLmax, NUmax, itotal_fl_l, itotal_fl_u,                    &
!     &      (NLmax*np_smp), (NUmax*np_smp), NHYP, np_smp,              &
!     &      inter_smp_stack, STACKmc, NLmaxHYP, NUmaxHYP, NEWtoOLD,    &
!     &      OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U, NEWtoOLD_DJDS_U,         &
!     &      LtoU, indexDJDS_L, indexDJDS_U, itemDJDS_L, itemDJDS_U,    &
!     &      Cmat_DJDS%D, Cmat_DJDS%AL, Cmat_DJDS%AU, my_rank)
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
        call solve_DJDS_kemo                                            &
     &     (internal_node, numnod, NLmax, NUmax,                        &
     &      itotal_fl_l, itotal_fl_u, NHYP, np_smp, inter_smp_stack,    &
     &      STACKmc, NLmaxHYP, NUmaxHYP, IVECT,  NEWtoOLD,              &
     &      OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U, NEWtoOLD_DJDS_U, LtoU,    &
     &      Cmat_DJDS%D, b_vec(1), x_vec(1),                            &
     &      indexDJDS_L, indexDJDS_U,  itemDJDS_L, itemDJDS_U,          &
     &      Cmat_DJDS%AL, Cmat_DJDS%AU,                                 &
     &      Cmat_DJDS%ALUG_L, Cmat_DJDS%ALUG_U, eps_4_d_scalar_crank,   &
     &      itr, ierr, neigh_pe_num_fl, neigh_pe_data_fl,               &
     &      istack_import_fl, item_import_fl,                           &
     &      istack_export_fl, NOD_EXPORT_NEW_fl,                        &
     &      method_4_solver, precond_4_solver, itr_res)
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
