!wider_radial_filter_data.f90
!      module wider_radial_filter_data
!
!     Written by H. Matsui on Nov., 2006
!
!!      subroutine cal_wider_fileters                                   &
!!     &         (sph_rj, r_filter, wide_filter, wide2_filter)
!
!
      module wider_radial_filter_data
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_filter_coefficients
      use t_spheric_mesh
      use t_spheric_parameter
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_wider_fileters                                     &
     &         (sph_rj, r_filter, wide_filter, wide2_filter)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(filter_coefficients_type), intent(in) :: r_filter
!
      type(filter_coefficients_type), intent(inout) :: wide_filter
      type(filter_coefficients_type), intent(inout) :: wide2_filter
!
      integer(kind = kint) :: nri
      real(kind = kreal), allocatable :: a_org(:,:), a_prod(:,:)
      real(kind = kreal), allocatable :: a_prd2(:,:)
!
!
      nri =  sph_rj%nidx_rj(1)
      allocate(a_org(nri,nri), a_prod(nri,nri), a_prd2(nri,nri))
!
      call set_filter_to_matrix(nri, r_filter, a_org)
!
      call const_wider_radial_fileter                                   &
     &   (nri, a_org, a_org, a_prod, wide_filter)
!
      call const_wider_radial_fileter                                   &
     &   (nri, a_org, a_prod, a_prd2, wide2_filter)
!
      deallocate(a_org, a_prod, a_prd2)
!
      end subroutine cal_wider_fileters
!
! -----------------------------------------------------------------------
!
      subroutine const_wider_radial_fileter                             &
     &         (nri, a1, a2, a_prod, wide_filter)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: a1(nri,nri)
      real(kind = kreal), intent(in) :: a2(nri,nri)
      real(kind = kreal), intent(inout) :: a_prod(nri,nri)
      type(filter_coefficients_type), intent(inout) :: wide_filter
!
!
      a_prod = matmul(a1, a2)
!
      wide_filter%ngrp_node = 1
      call alloc_num_filtering_comb(np_smp, wide_filter)
      wide_filter%group_name(1) = 'outer_core'
!
      call count_r_point_wide_filter(nri, a_prod, wide_filter)
!
      call alloc_inod_filter_comb(wide_filter)
!
      call count_wide_fiiltering_area(nri, a_prod, wide_filter)
!
      call alloc_3d_filter_comb(wide_filter)
!
      call set_wider_filter(nri, a_prod, wide_filter)
!
      end subroutine const_wider_radial_fileter
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_filter_to_matrix(nri, r_filter, a_org)
!
      integer(kind = kint), intent(in) :: nri
      type(filter_coefficients_type), intent(in) :: r_filter
      real(kind = kreal), intent(inout) :: a_org(nri,nri)
!
      integer(kind = kint) :: inum, inod, jst, jed, jnum, jnod
!
!
      do inum = 1, r_filter%num_node(1)
        inod = r_filter%inod_filter(inum)
        jst =  r_filter%istack_near_nod(inum-1) + 1
        jed =  r_filter%istack_near_nod(inum)
        do jnum = jst, jed
          jnod = r_filter%inod_near(jnum)
          a_org(inod,jnod) = r_filter%weight(jnum)
        end do
      end do
!
      end subroutine set_filter_to_matrix
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_r_point_wide_filter                              &
     &         (nri, a_prod, prod_filter)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: a_prod(nri,nri)
      type(filter_coefficients_type), intent(inout) :: prod_filter
!
      integer(kind = kint) :: inod, icou, jnod
!
!
      prod_filter%num_node(1) = 0
      do inod = 1, nri
        icou = 0
        do jnod = 1, nri
          if(a_prod(inod,jnod) .ne. zero) then
            icou = 1
            exit
          end if
        end do
        prod_filter%num_node(1) = prod_filter%num_node(1) + icou
      end do
      prod_filter%istack_node(1) = prod_filter%num_node(1)
      prod_filter%ntot_nod =       prod_filter%istack_node(1)
!
      end subroutine count_r_point_wide_filter
!
! ----------------------------------------------------------------------
!
      subroutine count_wide_fiiltering_area                             &
     &         (nri, a_prod, prod_filter)
!
      use quicksort
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: a_prod(nri,nri)
      type(filter_coefficients_type), intent(inout) :: prod_filter
!
      integer(kind = kint) :: inum, inod, icou, jnod, num
!
!
      do inod = 1, nri
        icou = prod_filter%istack_node(0)
        do jnod = 1, nri
          if(a_prod(inod,jnod) .ne. zero) then
            icou = icou + 1
            prod_filter%inod_filter(icou) = inod
            exit
          end if
        end do
      end do
!
      num = prod_filter%num_node(1)
!$omp parallel do private(inum,inod,icou,jnod)
      do inum = 1, num
        inod =  prod_filter%inod_filter(inum)
        icou = 0
        do jnod = 1, nri
          if(a_prod(inod,jnod) .ne. zero) icou = icou + 1
        end do
        prod_filter%nnod_near(inum) =  icou
      end do
!$omp end parallel do
!
      do inum = 1, num
        prod_filter%istack_near_nod(inum)                               &
     &    = prod_filter%istack_near_nod(inum-1)                         &
     &     + prod_filter%nnod_near(inum)
      end do
      prod_filter%ntot_near_nod = prod_filter%istack_near_nod(num)
!
      end subroutine count_wide_fiiltering_area
!
! ----------------------------------------------------------------------
!
      subroutine set_wider_filter(nri, a_prod, prod_filter)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: a_prod(nri,nri)
      type(filter_coefficients_type), intent(inout) :: prod_filter
!
      integer(kind = kint) :: inum, inod, icou, jnod
!
!
!$omp parallel do private(inum,inod,icou,jnod)
      do inum = 1, prod_filter%num_node(1)
        inod = prod_filter%inod_filter(inum)
        icou = prod_filter%istack_near_nod(inum-1)  
        do jnod = 1, nri
          if(a_prod(inod,jnod) .ne. zero) then
            icou = icou + 1
            prod_filter%inod_filter(icou) = jnod
            prod_filter%weight(icou) = a_prod(inod,jnod)
          end if
        end do
      end do
!$omp end parallel do
!
      end subroutine set_wider_filter
!
! -----------------------------------------------------------------------
!
      end module wider_radial_filter_data
