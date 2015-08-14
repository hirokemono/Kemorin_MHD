!
!      module solve_by_mass_z
!
!     Written by H. Matsui
!     Modified by H. Matsui on Apr., 2008
!
!     solve using CRS matrix
!
!      subroutine solve_crs_by_mass_z
!      subroutine solve_crs_by_mass_z2
!
      module solve_by_mass_z
!
      use m_geometry_data
      use m_int_edge_vart_width
      use m_consist_mass_crs
!
      use m_precision
!
      implicit none
!
      real(kind = kreal) :: rtime, starttime, endtime
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine solve_crs_by_mass_z
!
      use calypso_mpi
      use m_crs_connect
      use m_crs_matrix
      use DJDS_precond_solve11
!
      integer(kind = kint) :: i, ierr
!
!
      NB_crs = 1
         write(*,*) 'allocate_crs_mat_data'
      call allocate_crs_mat_data(node1%numnod)
!
      do i = 1, node1%numnod
        D_crs(1,1,i) = d_mk_crs(i)
        B_crs(i) = rhs_mk_crs(i)
      end do
      do i = 1, ntot_crs_l
        AL_crs(1,1,i) = al_mk_crs(i)
      end do
      do i = 1, ntot_crs_u
        AU_crs(1,1,i) = au_mk_crs(i)
      end do
!
         write(*,*) 'solve_by_djds_solver11'
      call solve_by_djds_solver11(ierr)
!
      do i = 1, node1%numnod
        sol_mk_crs(i) = X_crs(i)
      end do
!
         write(*,*) 'deallocate_crs_mat_data'
      call deallocate_crs_mat_data
!
      end subroutine solve_crs_by_mass_z
!
!  ---------------------------------------------------------------------
!
      subroutine solve_crs_by_mass_z2
!
      use calypso_mpi
      use m_machine_parameter
      use m_nod_comm_table
      use m_geometry_data
      use m_iccg_parameter
      use m_solver_djds
      use m_matrix_data_4_djds
      use m_crs_connect
      use m_crs_matrix
!
      use copy_matrix_2_djds_array
      use solver_DJDS
!
      integer(kind = kint) :: i, ierr
!
!
      NB_crs = 1
         write(*,*) 'allocate_crs_mat_data'
      call allocate_crs_mat_data(node1%numnod)
!
      do i = 1, node1%numnod
        B_crs(i) = rhs_mk_crs(i)
      end do
!
      call copy_RH_vect_2_crs_nn(node1%numnod)
!
        write(*,*) 'init_solve_DJDS_kemo'
      call init_solve_DJDS_kemo(node1%internal_node, node1%numnod,      &
     &     NLmax, NUmax, itotal_l, itotal_u, NHYP, np_smp,              &
     &     node1%istack_internal_smp, STACKmc, NLmaxHYP, NUmaxHYP,      &
     &     IVECT, NEWtoOLD, OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U,           &
     &     NEWtoOLD_DJDS_U, LtoU, aiccg(im_d), b_djds, x_djds,          &
     &     indexDJDS_L, indexDJDS_U, itemDJDS_L, itemDJDS_U,            &
     &     aiccg(im_l), aiccg(im_u),                                    &
     &     ALUG_L, ALUG_U, eps, itr, ierr,                              &
     &     nod_comm%num_neib, nod_comm%id_neib,                         &
     &     nod_comm%istack_import, nod_comm%item_import,                &
     &     nod_comm%istack_export, NOD_EXPORT_NEW,                      &
     &     method_4_solver, precond_4_solver, itr_res)

      call copy_solution_2_crs_nn(node1%numnod)
!
      do i = 1, node1%numnod
        sol_mk_crs(i) = X_crs(i)
      end do
!
         write(*,*) 'deallocate_crs_mat_data'
      call deallocate_crs_mat_data
!
      end subroutine solve_crs_by_mass_z2
!
!  ---------------------------------------------------------------------
!
      end module solve_by_mass_z
