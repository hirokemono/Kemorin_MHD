!>@file   sum_sph_rms_data.f90
!!@brief      module sum_sph_rms_data
!!
!!@author H. Matsui
!!@date Programmed in 2009
!
!> @brief  Evaluate mean square by spherical hermonics coefficients
!!
!!@verbatim
!!      subroutine deallocate_rms_sph_local_data
!!      subroutine set_sum_table_4_sph_spectr
!!      subroutine sum_sph_layerd_rms
!!@endverbatim
!
      module sum_sph_rms_data
!
      use m_precision
      use m_constants
!
      implicit none
!
      real(kind = kreal), allocatable :: rms_sph_rj(:,:,:)
!
      real(kind = kreal), allocatable :: rms_sph_vol_j(:,:)
!
      integer(kind = kint), allocatable :: num_mode_sum_l(:)
      integer(kind = kint), allocatable :: num_mode_sum_m(:)
      integer(kind = kint), allocatable :: num_mode_sum_lm(:)
      integer(kind = kint), allocatable :: istack_mode_sum_l(:)
      integer(kind = kint), allocatable :: istack_mode_sum_m(:)
      integer(kind = kint), allocatable :: istack_mode_sum_lm(:)
!
      integer(kind = kint), allocatable :: item_mode_sum_l(:)
      integer(kind = kint), allocatable :: item_mode_sum_m(:)
      integer(kind = kint), allocatable :: item_mode_sum_lm(:)
!
      real(kind = kreal), allocatable :: rms_sph_l_local(:,:,:)
      real(kind = kreal), allocatable :: rms_sph_m_local(:,:,:)
      real(kind = kreal), allocatable :: rms_sph_lm_local(:,:,:)
!
      real(kind = kreal), allocatable :: rms_sph_vl_local(:,:)
      real(kind = kreal), allocatable :: rms_sph_vm_local(:,:)
      real(kind = kreal), allocatable :: rms_sph_vlm_local(:,:)
!
      private :: rms_sph_rj
      private :: num_mode_sum_l,  istack_mode_sum_l,  item_mode_sum_l
      private :: num_mode_sum_m,  istack_mode_sum_m,  item_mode_sum_m
      private :: num_mode_sum_lm, istack_mode_sum_lm, item_mode_sum_lm
!
      private :: rms_sph_l_local, rms_sph_m_local, rms_sph_lm_local
!
      private :: allocate_rms_sph_local_data
      private :: sum_sph_rms_by_degree, sum_sph_rms_all_modes
      private :: sum_sph_v_rms_by_degree
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_rms_sph_local_data
!
      use m_spheric_parameter
      use m_rms_4_sph_spectr
!
      integer(kind = kint) :: nri, jmax
!
!
      nri =  nidx_rj(1)
      jmax = nidx_rj(2)
      allocate( rms_sph_rj(0:nri,jmax,3) )
      rms_sph_rj = 0.0d0
!
      allocate( rms_sph_vol_j(jmax,3) )
      rms_sph_vol_j = 0.0d0
!
      allocate( rms_sph_l_local(0:nri,0:l_truncation,ntot_rms_rj) )
      allocate( rms_sph_m_local(0:nri,0:l_truncation,ntot_rms_rj) )
      allocate( rms_sph_lm_local(0:nri,0:l_truncation,ntot_rms_rj) )
      if(ntot_rms_rj .gt. 0) then
        rms_sph_l_local = 0.0d0
        rms_sph_m_local = 0.0d0
        rms_sph_lm_local = 0.0d0
      end if
!
      allocate( rms_sph_vl_local(0:l_truncation,ntot_rms_rj) )
      allocate( rms_sph_vm_local(0:l_truncation,ntot_rms_rj) )
      allocate( rms_sph_vlm_local(0:l_truncation,ntot_rms_rj) )
      if(ntot_rms_rj .gt. 0) then
        rms_sph_vl_local = 0.0d0
        rms_sph_vm_local = 0.0d0
        rms_sph_vlm_local = 0.0d0
      end if
!
      allocate( num_mode_sum_l(0:l_truncation) )
      allocate( num_mode_sum_m(0:l_truncation) )
      allocate( num_mode_sum_lm(0:l_truncation) )
      allocate( istack_mode_sum_l(-1:l_truncation) )
      allocate( istack_mode_sum_m(-1:l_truncation) )
      allocate( istack_mode_sum_lm(-1:l_truncation) )
      allocate( item_mode_sum_l(jmax) )
      allocate( item_mode_sum_m(jmax) )
      allocate( item_mode_sum_lm(jmax) )
!
      num_mode_sum_l =      0
      num_mode_sum_m =      0
      num_mode_sum_lm =     0
      istack_mode_sum_l =   0
      istack_mode_sum_m =   0
      istack_mode_sum_lm =  0
      item_mode_sum_l =     0
      item_mode_sum_m =     0
      item_mode_sum_lm =    0
!
      end subroutine allocate_rms_sph_local_data
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_rms_sph_local_data
!
!
      deallocate(rms_sph_rj, rms_sph_vol_j)
      deallocate(rms_sph_l_local, rms_sph_m_local, rms_sph_lm_local)
      deallocate(rms_sph_vl_local, rms_sph_vm_local, rms_sph_vlm_local)
!
      deallocate(num_mode_sum_l,  istack_mode_sum_l,  item_mode_sum_l )
      deallocate(num_mode_sum_m,  istack_mode_sum_m,  item_mode_sum_m )
      deallocate(num_mode_sum_lm, istack_mode_sum_lm, item_mode_sum_lm)
!
      end subroutine deallocate_rms_sph_local_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_sum_table_4_sph_spectr
!
      use m_spheric_parameter
      use m_rms_4_sph_spectr
!
      integer(kind = kint) :: j, lg, mg, lm
      integer(kind = kint) :: icou, lcou, mcou
!
!
     call allocate_rms_sph_local_data
!
      num_mode_sum_l(0:l_truncation) =  0
      num_mode_sum_m(0:l_truncation) =  0
      num_mode_sum_lm(0:l_truncation) = 0
      do j = 1, nidx_rj(2)
        lg = idx_gl_1d_rj_j(j,2)
        mg = idx_gl_1d_rj_j(j,3)
        mg = abs(mg)
        lm = lg - mg
        num_mode_sum_l(lg) =  num_mode_sum_l(lg) +  1
        num_mode_sum_m(mg) =  num_mode_sum_m(mg) +  1
        num_mode_sum_lm(lm) = num_mode_sum_lm(lm) + 1
      end do
!
      istack_mode_sum_l(-1) =  0
      istack_mode_sum_m(-1) =  0
      istack_mode_sum_lm(-1) = 0
      do lm = 0, l_truncation
        istack_mode_sum_l(lm) = istack_mode_sum_l(lm-1)                 &
     &                         + num_mode_sum_l(lm)
        istack_mode_sum_m(lm) = istack_mode_sum_m(lm-1)                 &
     &                         + num_mode_sum_m(lm)
        istack_mode_sum_lm(lm) = istack_mode_sum_lm(lm-1)               &
     &                         + num_mode_sum_lm(lm)
      end do
!
      do lm = 0, l_truncation
        lcou = istack_mode_sum_l(lm-1)
        mcou = istack_mode_sum_m(lm-1)
        icou = istack_mode_sum_lm(lm-1)
        do j = 1, nidx_rj(2)
          lg = idx_gl_1d_rj_j(j,2)
          mg = idx_gl_1d_rj_j(j,3)
          mg = abs(mg)
          if (lg .eq. lm) then
            lcou = lcou + 1
            item_mode_sum_l(lcou) = j
          end if
          if (mg .eq. lm) then
            mcou = mcou + 1
            item_mode_sum_m(mcou) = j
          end if
          if ((lg-mg) .eq. lm) then
            icou = icou + 1
            item_mode_sum_lm(icou) = j
          end if
        end do
      end do
!
      end subroutine set_sum_table_4_sph_spectr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sum_sph_layerd_rms(kg_st, kg_ed)
!
      use calypso_mpi
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
      use cal_rms_by_sph_spectr
      use radial_int_for_sph_spec
!
      integer(kind = kint), intent(in) :: kg_st, kg_ed
!
      integer(kind = kint) :: j_fld, i_fld
      integer(kind = kint) :: icomp_rj, jcomp_st, ncomp_rj
      integer(kind = kint) :: num
!
!
      rms_sph_l_local = zero
      rms_sph_m_local = zero
      rms_sph_lm_local = zero
      rms_sph_vl_local = zero
      rms_sph_vm_local = zero
      rms_sph_vlm_local = zero
!
      do j_fld = 1, num_rms_rj
        i_fld = ifield_rms_rj(j_fld)
        icomp_rj = istack_phys_comp_rj(i_fld-1) + 1
        jcomp_st = istack_rms_comp_rj(j_fld-1) + 1
        ncomp_rj = num_rms_comp_rj(j_fld)
        num = nidx_rj(2) * ncomp_rj
        call cal_rms_sph_spec_one_field(ncomp_rj, icomp_rj,             &
     &      nidx_rj(1), nidx_rj(2), rms_sph_rj(0,1,1))
        call radial_integration(kg_st, kg_ed, nidx_rj(1),               &
     &      radius_1d_rj_r, num, rms_sph_rj(0,1,1), rms_sph_vol_j(1,1))
!
        call sum_sph_v_rms_by_degree(l_truncation, nidx_rj(2),          &
     &      istack_mode_sum_l,  item_mode_sum_l,  ncomp_rj,             &
     &      rms_sph_vl_local(0,jcomp_st))
        call sum_sph_v_rms_by_degree(l_truncation, nidx_rj(2),          &
     &      istack_mode_sum_m,  item_mode_sum_m,  ncomp_rj,             &
     &      rms_sph_vm_local(0,jcomp_st))
        call sum_sph_v_rms_by_degree(l_truncation, nidx_rj(2),          &
     &      istack_mode_sum_lm, item_mode_sum_lm, ncomp_rj,             &
     &      rms_sph_vlm_local(0,jcomp_st))
!
        call sum_sph_rms_by_degree(l_truncation,                        &
     &      nidx_rj(1), nidx_rj(2), inod_rj_center, idx_rj_degree_zero, &
     &      istack_mode_sum_l,  item_mode_sum_l, ncomp_rj,              &
     &      rms_sph_l_local(0,0,jcomp_st))
        call sum_sph_rms_by_degree(l_truncation,                        &
     &      nidx_rj(1), nidx_rj(2), inod_rj_center, idx_rj_degree_zero, &
     &      istack_mode_sum_m,  item_mode_sum_m, ncomp_rj,              &
     &      rms_sph_m_local(0,0,jcomp_st))
        call sum_sph_rms_by_degree(l_truncation,                        &
     &      nidx_rj(1), nidx_rj(2), inod_rj_center, idx_rj_degree_zero, &
     &      istack_mode_sum_lm, item_mode_sum_lm, ncomp_rj,             &
     &      rms_sph_lm_local(0,0,jcomp_st))
      end do
!
      num = ntot_rms_rj * (l_truncation + 1)
      call MPI_REDUCE (rms_sph_vl_local, rms_sph_vol_l,                 &
     &    num, CALYPSO_REAL, MPI_SUM, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_REDUCE (rms_sph_vm_local, rms_sph_vol_m,                 &
     &    num, CALYPSO_REAL, MPI_SUM, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_REDUCE (rms_sph_vlm_local, rms_sph_vol_lm,               &
     &    num, CALYPSO_REAL, MPI_SUM, izero, CALYPSO_COMM, ierr_MPI)
!
      num = ntot_rms_rj * (nidx_rj(1) + 1) * (l_truncation + 1)
      call MPI_REDUCE (rms_sph_l_local, rms_sph_l, num, CALYPSO_REAL,   &
     &    MPI_SUM, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_REDUCE (rms_sph_m_local, rms_sph_m, num, CALYPSO_REAL,   &
     &    MPI_SUM, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_REDUCE (rms_sph_lm_local, rms_sph_lm, num, CALYPSO_REAL, &
     &    MPI_SUM, izero, CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .gt. 0) return
!
      call sum_sph_vol_rms_all_modes(l_truncation, ntot_rms_rj,         &
     &    rms_sph_vol_l(0,1), rms_sph_vol(1) )
!
      call sum_sph_rms_all_modes(l_truncation, nidx_rj(1),              &
     &    ntot_rms_rj, rms_sph_l(0,0,1), rms_sph(0,1) )
!
      end subroutine sum_sph_layerd_rms
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sum_sph_v_rms_by_degree(ltr, nidx_j,                   &
     &          istack_sum, item_mode_4_sum, ncomp, rms_sph_lc)
!
      integer(kind = kint), intent(in) :: ltr, nidx_j
      integer(kind = kint), intent(in) :: ncomp
!
      integer(kind = kint), intent(in) :: istack_sum(-1:ltr)
      integer(kind = kint), intent(in) :: item_mode_4_sum(nidx_j)
!
      real(kind = kreal), intent(inout) :: rms_sph_lc(0:ltr,ncomp)
!
      integer(kind = kint) :: lm, j, l0, icomp
      integer(kind = kint) :: lst, led
!
!
!$omp parallel private(icomp)
      do icomp = 1, ncomp
!$omp do private(lm,lst,led,l0,j)
        do lm = 0, ltr
          lst = istack_sum(lm-1) + 1
          led = istack_sum(lm)
          do l0 = lst, led
            j = item_mode_4_sum(l0)
            rms_sph_lc(lm,icomp) = rms_sph_lc(lm,icomp)                 &
     &                                + rms_sph_vol_j(j,icomp)
          end do
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine sum_sph_v_rms_by_degree
!
! -----------------------------------------------------------------------
!
      subroutine sum_sph_rms_by_degree(ltr, nidx_r, nidx_j,             &
     &          inod_rj_center, idx_rj_degree_zero,                     &
     &          istack_sum, item_mode_4_sum, ncomp, rms_sph_lc)
!
      integer(kind = kint), intent(in) :: ltr, nidx_r, nidx_j
      integer(kind = kint), intent(in) :: ncomp, inod_rj_center
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
!
      integer(kind = kint), intent(in) :: istack_sum(-1:ltr)
      integer(kind = kint), intent(in) :: item_mode_4_sum(nidx_j)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: rms_sph_lc(0:nidx_r,0:ltr,ncomp)
!
      integer(kind = kint) :: lm, k, j, l0, icomp
      integer(kind = kint) :: lst, led
!
!
!$omp parallel private(icomp)
      do icomp = 1, ncomp
!$omp do private(k,lm,lst,led,l0,j)
        do k = 1, nidx_r
          do lm = 0, ltr
            lst = istack_sum(lm-1) + 1
            led = istack_sum(lm)
            do l0 = lst, led
              j = item_mode_4_sum(l0)
              rms_sph_lc(k,lm,icomp) = rms_sph_lc(k,lm,icomp)           &
     &                                + rms_sph_rj(k,j,icomp)
            end do
          end do
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      if(inod_rj_center .eq. 0) return
      j = idx_rj_degree_zero
      do icomp = 1, ncomp
        rms_sph_lc(0,0,icomp) = rms_sph_rj(0,j,icomp)
      end do
!
      end subroutine sum_sph_rms_by_degree
!
! -----------------------------------------------------------------------
!
      subroutine sum_sph_vol_rms_all_modes(ltr, ntot_rms,           &
     &          rms_sph_vl, rms_v_sph)
!
      integer(kind = kint), intent(in) :: ltr, ntot_rms
      real(kind = kreal), intent(in) :: rms_sph_vl(0:ltr,ntot_rms)
!
      real(kind = kreal), intent(inout) :: rms_v_sph(ntot_rms)
!
      integer(kind = kint) :: lm, nd
!
!
!$omp parallel do private(lm,nd)
      do nd = 1, ntot_rms
        rms_v_sph(nd) = rms_sph_vl(0,nd)
!
        do lm = 1, ltr
          rms_v_sph(nd) = rms_v_sph(nd) + rms_sph_vl(lm,nd)
        end do
      end do
!$omp end parallel do
!
      end subroutine sum_sph_vol_rms_all_modes
!
! -----------------------------------------------------------------------
!
      subroutine sum_sph_rms_all_modes(ltr, nidx_r, ntot_rms,           &
     &          rms_sph_l, rms_sph)
!
      integer(kind = kint), intent(in) :: ltr, nidx_r
      integer(kind = kint), intent(in) :: ntot_rms
      real(kind = kreal), intent(in)                                    &
     &                   :: rms_sph_l(0:nidx_r,0:ltr,ntot_rms)
!
      real(kind = kreal), intent(inout) :: rms_sph(0:nidx_r,ntot_rms)
!
      integer(kind = kint) :: lm, k, nd
!
!
!$omp parallel do private(k,lm,nd)
      do nd = 1, ntot_rms
        do k = 0, nidx_r
          rms_sph(k,nd) = rms_sph_l(k,0,nd)
        end do
!
        do k = 1, nidx_r
          do lm = 1, ltr
            rms_sph(k,nd) = rms_sph(k,nd) + rms_sph_l(k,lm,nd)
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine sum_sph_rms_all_modes
!
! -----------------------------------------------------------------------
!
      end module sum_sph_rms_data
