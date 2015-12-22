!cal_average_mag_potential.f90
!      module cal_average_mag_potential
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!      Modified by H. Matsui on Aug, 2007
!
!      subroutine s_cal_average_mag_potential
!
      module cal_average_mag_potential
!
      use m_precision
!
      implicit none
!
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_cal_average_mag_potential
!
      use calypso_mpi
      use m_control_parameter
      use m_geometry_data
      use m_geometry_data_MHD
      use m_jacobians
      use m_node_phys_data
      use m_bulk_values
!
!
      if ( inner_core%numele_fld .eq. 0 ) return
!
        call fem_icore_mag_potential_icore(node1%numnod,                &
     &      ele1%numele, ele1%nnod_4_ele, ele1%ie, ele1%interior_ele,   &
     &      inner_core%numele_fld, inner_core%istack_ele_fld_smp,       &
     &      inner_core%iele_fld, jac1_3d_l%ntot_int, intg_point_t_evo,  &
     &      jac1_3d_l%xjac, jac1_3d_l%an,                               &
     &      nod_fld1%ntot_phys, nod_fld1%d_fld, iphys%i_mag_p,          &
     &      ave_mp_core_local)
!
        call MPI_allREDUCE (ave_mp_core_local, ave_mp_core, 1,          &
     &       CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
        ave_mp_core = ave_mp_core * inner_core%volume
        if (my_rank.eq.0) write(84,*) ' ave_mp: ', ave_mp_core
!
      end subroutine s_cal_average_mag_potential
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine fem_icore_mag_potential_icore                          &
     &         (numnod, numele, nnod_4_ele, ie, interior_ele,           &
     &          numele_in_core, iele_in_core_smp_stack, iele_in_core,   &
     &          ntot_int_3d, n_int, xjac, an, ntot_phys, d_nod, i_mphi, &
     &          ave_mp_core_local)
!
      use m_machine_parameter
      use m_geometry_constants
      use m_fem_gauss_int_coefs
!
      integer(kind=kint), intent(in) :: numele, nnod_4_ele
      integer(kind=kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind=kint), intent(in) :: interior_ele(numele)
!
      integer(kind=kint), intent(in) :: numele_in_core
      integer(kind=kint), intent(in)                                    &
     &                    :: iele_in_core_smp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: iele_in_core(numele_in_core)
!
      integer (kind=kint), intent(in) :: ntot_int_3d, n_int
      real (kind=kreal), intent(in) :: xjac(numele,ntot_int_3d)
      real(kind=kreal), intent(in) :: an(num_t_linear,ntot_int_3d)
!
      integer(kind=kint), intent(in) :: numnod, ntot_phys, i_mphi
      real(kind = kreal), intent(in) :: d_nod(numnod,ntot_phys)
!
      real(kind = kreal), intent(inout) :: ave_mp_core_local
!
      real (kind=kreal) :: bulk_e_smp(np_smp)
!
      integer (kind=kint):: inod1, inod2, ist, ied, k1
      integer (kind = kint) :: iproc, ii, ix, inod, iele, inum
!
!
        ave_mp_core_local = 0.0d0
        bulk_e_smp = 0.0d0
!
!$omp parallel do private(k1,iele,ist,ied,inod1,inod2) 
      do iproc = 1, np_smp
        ist = iele_in_core_smp_stack(iproc-1)+1
        ied = iele_in_core_smp_stack(iproc)
!
        do k1 = 1, num_t_linear
          do ii= 1, n_int * n_int * n_int 
            ix = int_start3(n_int) + ii
!
            do inum = ist, ied
              iele = iele_in_core(inum)
              inod = ie(iele,k1)
!
              bulk_e_smp(iproc) = bulk_e_smp(iproc)                     &
     &                      + dble(interior_ele(iele))                  &
     &                       * d_nod(inod,i_mphi)                       &
     &                       * an(k1,ix)*xjac(iele,ix)*owe3d(ix)
            end do
          end do
         end do
      end do
!$omp end parallel do
!
      do iproc = 1, np_smp
        ave_mp_core_local = ave_mp_core_local + bulk_e_smp(iproc)
      end do
!
      end subroutine fem_icore_mag_potential_icore
!
! ----------------------------------------------------------------------
!
      end module cal_average_mag_potential