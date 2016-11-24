!cal_int_sph_nonlinear.f90
!      module cal_int_sph_nonlinear
!
!     Written by H. Matsui on March, 2010
!
!      subroutine s_cal_int_sph_nonlinear(iflag_db, np_smp,             &
!     &          ltr, jmax, idx_gl, gaunt)
!      subroutine s_cal_int_sph_part_nl(iflag_db, np_smp,               &
!     &          ltr, jmax, idx_gl, nidx_3, idx_gl3, gaunt)
!!      type(adams_gaunt_integrals), intent(inout) :: gaunt
!
      module cal_int_sph_nonlinear
!
      use m_precision
!
      use t_integrals_sph_nonlinear
!
      implicit none
!
      integer(kind = kint), allocatable :: ist_j3_smp(:)
      integer(kind = kint), allocatable :: num_ki_smp(:)
      integer(kind = kint), allocatable :: ist_ki_smp(:)
!
      real(kind = kreal), allocatable ::  gi_12(:,:) ,ei_12(:,:)
!
      type(adams_gaunt_integrals) :: g_tmp
!
      private :: g_tmp
      private :: ist_j3_smp, num_ki_smp, ist_ki_smp
      private :: gi_12, ei_12
      private :: init_cal_int_sph_nonlinear
      private :: cal_gaunt_int_nl, cal_elsasser_int_nl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_cal_int_sph_nonlinear(iflag_db, np_smp,              &
     &          ltr, jmax, idx_gl, gaunt)
!
      integer(kind = kint), intent(in) :: iflag_db
      integer(kind = kint), intent(in) :: np_smp, ltr, jmax
      integer(kind = kint), intent(in) :: idx_gl(jmax,3)
!
      type(adams_gaunt_integrals), intent(inout) :: gaunt
!
!
      call alloc_gi_stack_nl(ltr, jmax, gaunt)
      call alloc_gi_for_nl(gaunt)
      call alloc_ei_for_nl(gaunt)
!
!
      call init_cal_int_sph_nonlinear(iflag_db, np_smp, gaunt)
!
      call cal_gaunt_int_nl(iflag_db, np_smp, jmax, idx_gl,            &
     &    jmax, idx_gl, gaunt)
      call cal_elsasser_int_nl(iflag_db, np_smp, jmax, idx_gl,         &
     &    jmax, idx_gl, gaunt)
      gaunt%ntot_larger_gei_nl_lm3                                     &
     &         = max(gaunt%max_j12_gi, gaunt%max_j12_ei)
!
      deallocate(gi_12, ei_12)
      deallocate(ist_j3_smp, num_ki_smp, ist_ki_smp)
!
      end subroutine s_cal_int_sph_nonlinear
!
!  ---------------------------------------------------------------------
!
      subroutine s_cal_int_sph_part_nl(iflag_db, np_smp,                &
     &          ltr, jmax, idx_gl, nidx_3, idx_gl3, gaunt)
!
      integer(kind = kint), intent(in) :: iflag_db
      integer(kind = kint), intent(in) :: np_smp, ltr, jmax
      integer(kind = kint), intent(in) :: idx_gl(jmax,3)
      integer(kind = kint), intent(in) :: nidx_3
      integer(kind = kint), intent(in) :: idx_gl3(nidx_3,3)
!
      type(adams_gaunt_integrals), intent(inout) :: gaunt
!
!
      call alloc_gi_stack_nl(ltr, jmax, gaunt)
      call alloc_gi_for_nl(gaunt)
      call alloc_ei_for_nl(gaunt)
!
!
      call init_cal_int_sph_nonlinear(iflag_db, np_smp, gaunt)
!
      call cal_gaunt_int_nl(iflag_db, np_smp, jmax, idx_gl,            &
     &    nidx_3, idx_gl3, gaunt)
      call cal_elsasser_int_nl(iflag_db, np_smp, jmax, idx_gl,         &
     &    nidx_3, idx_gl3, gaunt)
      gaunt%ntot_larger_gei_nl_lm3                                     &
     &        = max(gaunt%max_j12_gi, gaunt%max_j12_ei)
!
      deallocate(gi_12, ei_12)
      deallocate(ist_j3_smp, num_ki_smp, ist_ki_smp)
!
      end subroutine s_cal_int_sph_part_nl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine init_cal_int_sph_nonlinear(iflag_db, np_smp, gaunt)
!
      integer(kind = kint), intent(in) :: iflag_db, np_smp
      type(adams_gaunt_integrals), intent(in) :: gaunt
!
      integer(kind = kint) :: ip, jz, num
!
!
      allocate(gi_12(gaunt%jmax,gaunt%jmax))
      allocate(ei_12(gaunt%jmax,gaunt%jmax) )
!
      allocate(ist_j3_smp(0:np_smp))
      allocate(num_ki_smp(np_smp))
      allocate(ist_ki_smp(0:np_smp))
      num_ki_smp = 0
      ist_ki_smp = 0
!
      ist_j3_smp(0) = 0
      jz = mod(gaunt%jmax,np_smp)
      do ip = 1, jz
        ist_j3_smp(ip) = ist_j3_smp(ip-1) + (gaunt%jmax / np_smp) + 1
      end do
      do ip = jz+1, np_smp-1
        ist_j3_smp(ip) = ist_j3_smp(ip-1) + (gaunt%jmax / np_smp)
      end do
      ist_j3_smp(np_smp) = gaunt%jmax
!
      if (iflag_db .gt. 0) then
        do ip = 1, np_smp
          num = ist_j3_smp(ip)-ist_j3_smp(ip-1)
          write (6,*) ip, ist_j3_smp(ip), num
        end do
      end if
!
      end subroutine init_cal_int_sph_nonlinear
!
!  ---------------------------------------------------------------------
!
      subroutine cal_gaunt_int_nl(iflag_db, np_smp, jmax, idx_gl,       &
     &           nidx_3, idx_gl3, gaunt)
!
      use cal_gaunt_itgs
!
      integer(kind = kint), intent(in) :: iflag_db, np_smp, jmax
      integer(kind = kint), intent(in) :: idx_gl(jmax,3)
      integer(kind = kint), intent(in) :: nidx_3
      integer(kind = kint), intent(in) :: idx_gl3(nidx_3,3)
!
      type(adams_gaunt_integrals), intent(inout) :: gaunt
!
      integer(kind = kint) :: ip, ist, ied
      integer(kind = kint) :: l1, l2, l3, m1, m2, m3, j1, j2, j3, jz
      integer(kind = kint) :: lm1, lm2, j0
!
!
      do j0 = 1 ,nidx_3
        j3 = idx_gl3(j0,1)
        l3 = idx_gl3(j0,2)
        m3 = idx_gl3(j0,3)
        if(j3 .eq. 0) cycle
!
        if(iflag_db .gt. 0 .and. mod(j0,icent).eq.0 ) then
          write(*,*) 'set Gaunt for ',  j3, l3, m3
        end if
!
        num_ki_smp(1:np_smp) = 0
        ist_ki_smp(0) = 0
!$omp parallel do private(ist,ied,lm1,lm2,j1,j2,l1,l2,m1,m2)
        do ip = 1 ,np_smp
          ist = ist_j3_smp(ip-1)+1
          ied = ist_j3_smp(ip)
          do j1 = ist, ied
            lm1 = idx_gl(j1,1)
            l1 =  idx_gl(j1,2)
            m1 =  idx_gl(j1,3)
!*
            do j2 = 1 ,gaunt%jmax
              lm2 = idx_gl(j2,1)
              l2 =  idx_gl(j2,2)
              m2 =  idx_gl(j2,3)
!*
!*     ----  select & lead value  --------------------
!*
              gi_12(j2,j1) = leadki(l1, m1, l2, m2, l3, m3)
            end do
          end do
!
          do j1 = ist,ied
            do j2 = 1 ,gaunt%jmax
              if(abs(gi_12(j2,j1)) .ge. 5.0d-16)                        &
     &           num_ki_smp(ip) = num_ki_smp(ip) + 1
            end do
          end do
        end do
!$omp end parallel do
!
        do ip = 1 ,np_smp
          gaunt%num_gi_nl_lm3(j3,1) = gaunt%num_gi_nl_lm3(j3,1)         &
     &                               + num_ki_smp(ip)
          ist_ki_smp(ip) = ist_ki_smp(ip-1) + num_ki_smp(ip)
        end do
        gaunt%istack_gi_nl_lm3(j3,1) = gaunt%istack_gi_nl_lm3(j3-1,1)   &
     &                                + gaunt%num_gi_nl_lm3(j3,1)
!
        if(gaunt%num_gi_nl_lm3(j3,1) .gt. gaunt%max_j12_gi) then
          g_tmp%jmax = j3
          g_tmp%max_j12_gi = gaunt%max_j12_gi
          gaunt%max_j12_gi = gaunt%num_gi_nl_lm3(j3,1)
!
          call alloc_gi_for_nl(g_tmp)
!
!$omp parallel do
          do jz = 1, j3-1
            g_tmp%gi_nl2(1:g_tmp%max_j12_gi,jz)                         &
     &          = gaunt%gi_nl2(1:g_tmp%max_j12_gi,jz)
            g_tmp%lm_gi_nl2(1:g_tmp%max_j12_gi,1,jz)                    &
     &          = gaunt%lm_gi_nl2(1:g_tmp%max_j12_gi,1,jz)
            g_tmp%lm_gi_nl2(1:g_tmp%max_j12_gi,2,jz)                    &
     &          = gaunt%lm_gi_nl2(1:g_tmp%max_j12_gi,2,jz)
          end do
!$omp end parallel do
!
          call dealloc_gi_for_nl(gaunt)
          call alloc_gi_for_nl(gaunt)
!
!$omp parallel do
          do j1 = 1, j3-1
            gaunt%gi_nl2(1:g_tmp%max_j12_gi,j1)                         &
     &          = g_tmp%gi_nl2(1:g_tmp%max_j12_gi,j1)
            gaunt%lm_gi_nl2(1:g_tmp%max_j12_gi,1,j1)                    &
     &          = g_tmp%lm_gi_nl2(1:g_tmp%max_j12_gi,1,j1)
            gaunt%lm_gi_nl2(1:g_tmp%max_j12_gi,2,j1)                    &
     &          = g_tmp%lm_gi_nl2(1:g_tmp%max_j12_gi,2,j1)
          end do
!$omp end parallel do
!
          call dealloc_gi_for_nl(g_tmp)
        end if
!
!$omp parallel do private(ist,ied,j1,j2,jz)
        do ip = 1 ,np_smp
          ist = ist_j3_smp(ip-1)+1
          ied = ist_j3_smp(ip)
          num_ki_smp(ip) = ist_ki_smp(ip-1)
          do j1 = ist, ied
            do j2 = 1 ,gaunt%jmax
              if(abs(gi_12(j2,j1)) .ge. 5.0d-16) then
                num_ki_smp(ip) = num_ki_smp(ip) + 1
                jz = num_ki_smp(ip)
                gaunt%gi_nl2(jz,j3) = gi_12(j2,j1)
                gaunt%lm_gi_nl2(jz,1,j3) = j1
                gaunt%lm_gi_nl2(jz,2,j3) = j2
              end if
            end do
          end do
        end do
!$omp end parallel do
      end do
      gaunt%ntot_gi_nl_lm3 = gaunt%istack_gi_nl_lm3(gaunt%jmax,1)
!
      end subroutine cal_gaunt_int_nl
!
!  ---------------------------------------------------------------------
!
      subroutine cal_elsasser_int_nl(iflag_db, np_smp, jmax, idx_gl,    &
     &          nidx_3, idx_gl3, gaunt)
!
      use cal_gaunt_itgs
!
      integer(kind = kint), intent(in) :: iflag_db, np_smp, jmax
      integer(kind = kint), intent(in) :: idx_gl(jmax,3)
      integer(kind = kint), intent(in) :: nidx_3
      integer(kind = kint), intent(in) :: idx_gl3(nidx_3,3)
!
      type(adams_gaunt_integrals), intent(inout) :: gaunt
!
      integer(kind = kint) :: ip, ist, ied
      integer(kind = kint) :: l1, l2, l3, m1, m2, m3, j1, j2, j3, jz
      integer(kind = kint) :: lm1, lm2, j0
!
!
      do j0 = 1 ,nidx_3
        j3 = idx_gl3(j0,1)
        l3 = idx_gl3(j0,2)
        m3 = idx_gl3(j0,3)
        if(j3 .eq. 0) cycle
!
        if(iflag_db .gt. 0 .and. mod(j0,icent).eq.0 ) then
          write(*,*) 'set Elsasser for ',  j3, l3, m3
        end if
!*
        num_ki_smp(1:np_smp) = 0
        ist_ki_smp(0) = 0
!$omp parallel do private(ist,ied,lm1,lm2,j1,j2,l1,l2,m1,m2)
        do ip = 1 ,np_smp
          ist = ist_j3_smp(ip-1)+1
          ied = ist_j3_smp(ip)
          do j1 = ist, ied
            lm1 = idx_gl(j1,1)
            l1 =  idx_gl(j1,2)
            m1 =  idx_gl(j1,3)
!*
            do j2 = 1 ,gaunt%jmax
              lm2 = idx_gl(j2,1)
              l2 =  idx_gl(j2,2)
              m2 =  idx_gl(j2,3)
!*
!*     ----  select & lead value  --------------------
!*
              ei_12(j2,j1) = leadli(l1, m1, l2, m2, l3, m3)
            end do
          end do
!
          do j1 = ist,ied
            do j2 = 1 ,gaunt%jmax
              if(abs(ei_12(j2,j1)) .ge. 5.0d-16)                        &
     &           num_ki_smp(ip) = num_ki_smp(ip) + 1
            end do
          end do
        end do
!$omp end parallel do
!
        do ip = 1 ,np_smp
          gaunt%num_gi_nl_lm3(j3,2) = gaunt%num_gi_nl_lm3(j3,2)         &
     &                               + num_ki_smp(ip)
          ist_ki_smp(ip) = ist_ki_smp(ip-1) + num_ki_smp(ip)
        end do
        gaunt%istack_gi_nl_lm3(j3,2) = gaunt%istack_gi_nl_lm3(j3-1,2)   &
     &                                + gaunt%num_gi_nl_lm3(j3,2)
!
        if(gaunt%num_gi_nl_lm3(j3,2) .gt. gaunt%max_j12_ei) then
          g_tmp%jmax = j3
          g_tmp%max_j12_ei = gaunt%max_j12_ei
          gaunt%max_j12_ei = gaunt%num_gi_nl_lm3(j3,2)
!
          call alloc_ei_for_nl(g_tmp)
          do jz = 1, j3-1
            g_tmp%ei_nl2(1:g_tmp%max_j12_ei,jz)                         &
     &          =  gaunt%ei_nl2(1:g_tmp%max_j12_ei,jz)
            g_tmp%lm_ei_nl2(1:g_tmp%max_j12_ei,1,jz)                    &
     &          = gaunt%lm_ei_nl2(1:g_tmp%max_j12_ei,1,jz)
            g_tmp%lm_ei_nl2(1:g_tmp%max_j12_ei,2,jz)                    &
     &          = gaunt%lm_ei_nl2(1:g_tmp%max_j12_ei,2,jz)
          end do
!
          call dealloc_ei_for_nl(gaunt)
          call alloc_ei_for_nl(gaunt) 
!
          do j1 = 1, j3-1
            gaunt%ei_nl2(1:g_tmp%max_j12_ei,j1)                         &
     &          = g_tmp%ei_nl2(1:g_tmp%max_j12_ei,j1)
            gaunt%lm_ei_nl2(1:g_tmp%max_j12_ei,1,j1)                    &
     &          = g_tmp%lm_ei_nl2(1:g_tmp%max_j12_ei,1,j1)
            gaunt%lm_ei_nl2(1:g_tmp%max_j12_ei,2,j1)                    &
     &          = g_tmp%lm_ei_nl2(1:g_tmp%max_j12_ei,2,j1)
          end do
!
          call dealloc_ei_for_nl(g_tmp)
        end if
!
!
!$omp parallel do private(ist,ied,j1,j2,jz)
        do ip = 1 ,np_smp
         ist = ist_j3_smp(ip-1)+1
         ied = ist_j3_smp(ip)
         num_ki_smp(ip) = ist_ki_smp(ip-1)
         do j1 = ist, ied
          do j2 = 1 ,gaunt%jmax
            if(abs(ei_12(j2,j1)) .ge. 5.0d-16) then
              num_ki_smp(ip) = num_ki_smp(ip) + 1
              jz = num_ki_smp(ip)
              gaunt%ei_nl2(jz,j3) = ei_12(j2,j1)
              gaunt%lm_ei_nl2(jz,1,j3) = j1
              gaunt%lm_ei_nl2(jz,2,j3) = j2
            end if
          end do
         end do
        end do
!$omp end parallel do
      end do
      gaunt%ntot_ei_nl_lm3 = gaunt%istack_gi_nl_lm3(gaunt%jmax,2)
!
      end subroutine cal_elsasser_int_nl
!
!  ---------------------------------------------------------------------
!
      end module cal_int_sph_nonlinear
