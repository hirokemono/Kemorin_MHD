!cal_int_sph_nonlinear.f90
!      module cal_int_sph_nonlinear
!
!     Written by H. Matsui on March, 2010
!
!      subroutine s_cal_int_sph_nonlinear(iflag_db, np_smp,             &
!     &          ltr, jmax, idx_gl)
!      subroutine s_cal_int_sph_part_nl(iflag_db, np_smp,               &
!     &          ltr, jmax, idx_gl, nidx_3, idx_gl3)
!
      module cal_int_sph_nonlinear
!
      use m_precision
!
      use m_integrals_sph_nonlinear
!
      implicit none
!
      integer(kind = kint), allocatable :: ist_j3_smp(:)
      integer(kind = kint), allocatable :: num_ki_smp(:)
      integer(kind = kint), allocatable :: ist_ki_smp(:)
!
      real(kind = kreal), allocatable ::  gi_12(:,:) ,ei_12(:,:)
!
      integer(kind = kint), allocatable :: jid_t(:,:,:)
      real(kind = kreal), allocatable ::  di_tmp(:,:)
!
      private :: ist_j3_smp, num_ki_smp, ist_ki_smp
      private :: gi_12, ei_12, jid_t, di_tmp
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
     &          ltr, jmax, idx_gl)
!
      integer(kind = kint), intent(in) :: iflag_db
      integer(kind = kint), intent(in) :: np_smp, ltr, jmax
      integer(kind = kint), intent(in) :: idx_gl(jmax,3)
!
!
      ltr_gaunt =  ltr
      jmax_gaunt = jmax
      ntot_gi_nl_lm3 = 0
      ntot_ei_nl_lm3 = 0
      ntot_larger_gei_nl_lm3 = 0
      max_j12_gi = 1
      max_j12_ei = 1
      call allocate_gi_stack_nl
      call allocate_gi_for_nl
      call allocate_ei_for_nl
!
!
      call init_cal_int_sph_nonlinear(iflag_db, np_smp)
!
      call cal_gaunt_int_nl(iflag_db, np_smp, jmax, idx_gl,            &
     &    jmax, idx_gl)
      call cal_elsasser_int_nl(iflag_db, np_smp, jmax, idx_gl,         &
     &    jmax, idx_gl)
      ntot_larger_gei_nl_lm3 = max(max_j12_gi, max_j12_ei)
!
      deallocate(gi_12, ei_12)
      deallocate(ist_j3_smp, num_ki_smp, ist_ki_smp)
!
      end subroutine s_cal_int_sph_nonlinear
!
!  ---------------------------------------------------------------------
!
      subroutine s_cal_int_sph_part_nl(iflag_db, np_smp,                &
     &          ltr, jmax, idx_gl, nidx_3, idx_gl3)
!
      integer(kind = kint), intent(in) :: iflag_db
      integer(kind = kint), intent(in) :: np_smp, ltr, jmax
      integer(kind = kint), intent(in) :: idx_gl(jmax,3)
      integer(kind = kint), intent(in) :: nidx_3
      integer(kind = kint), intent(in) :: idx_gl3(nidx_3,3)
!
!
      ltr_gaunt =  ltr
      jmax_gaunt = jmax
      ntot_gi_nl_lm3 = 0
      ntot_ei_nl_lm3 = 0
      ntot_larger_gei_nl_lm3 = 0
      max_j12_gi = 1
      max_j12_ei = 1
      call allocate_gi_stack_nl
      call allocate_gi_for_nl
      call allocate_ei_for_nl
!
!
      call init_cal_int_sph_nonlinear(iflag_db, np_smp)
!
      call cal_gaunt_int_nl(iflag_db, np_smp, jmax, idx_gl,            &
     &    nidx_3, idx_gl3)
      call cal_elsasser_int_nl(iflag_db, np_smp, jmax, idx_gl,         &
     &    nidx_3, idx_gl3)
      ntot_larger_gei_nl_lm3 = max(max_j12_gi, max_j12_ei)
!
      deallocate(gi_12, ei_12)
      deallocate(ist_j3_smp, num_ki_smp, ist_ki_smp)
!
      end subroutine s_cal_int_sph_part_nl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine init_cal_int_sph_nonlinear(iflag_db, np_smp)
!
      integer(kind = kint), intent(in) :: iflag_db, np_smp
      integer(kind = kint) :: ip, jz, num
!
!
      allocate(gi_12(jmax_gaunt,jmax_gaunt))
      allocate(ei_12(jmax_gaunt,jmax_gaunt) )
!
      allocate(ist_j3_smp(0:np_smp))
      allocate(num_ki_smp(np_smp))
      allocate(ist_ki_smp(0:np_smp))
      num_ki_smp = 0
      ist_ki_smp = 0
!
      ist_j3_smp(0) = 0
      jz = mod(jmax_gaunt,np_smp)
      do ip = 1, jz
        ist_j3_smp(ip) = ist_j3_smp(ip-1) + (jmax_gaunt / np_smp) + 1
      end do
      do ip = jz+1, np_smp-1
        ist_j3_smp(ip) = ist_j3_smp(ip-1) + (jmax_gaunt / np_smp)
      end do
      ist_j3_smp(np_smp) = jmax_gaunt
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
     &           nidx_3, idx_gl3)
!
      use cal_gaunt_itgs
!
      integer(kind = kint), intent(in) :: iflag_db, np_smp, jmax
      integer(kind = kint), intent(in) :: idx_gl(jmax,3)
      integer(kind = kint), intent(in) :: nidx_3
      integer(kind = kint), intent(in) :: idx_gl3(nidx_3,3)
!
      integer(kind = kint) :: ip, ist, ied, max_j12_org
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
            do j2 = 1 ,jmax_gaunt
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
            do j2 = 1 ,jmax_gaunt
              if(abs(gi_12(j2,j1)) .ge. 5.0d-16)                        &
     &           num_ki_smp(ip) = num_ki_smp(ip) + 1
            end do
          end do
        end do
!$omp end parallel do
!
        do ip = 1 ,np_smp
          num_gi_nl_lm3(j3,1) = num_gi_nl_lm3(j3,1) + num_ki_smp(ip)
          ist_ki_smp(ip) = ist_ki_smp(ip-1) + num_ki_smp(ip)
        end do
        istack_gi_nl_lm3(j3,1) = istack_gi_nl_lm3(j3-1,1)               &
     &                          + num_gi_nl_lm3(j3,1)
!
        if( num_gi_nl_lm3(j3,1) .gt. max_j12_gi) then
          max_j12_org = max_j12_gi
          max_j12_gi = num_gi_nl_lm3(j3,1)
!
          allocate( di_tmp(max_j12_org,j3) )
          allocate( jid_t(max_j12_org,2,j3) )
!
!$omp parallel do
          do jz = 1, j3-1
            di_tmp(1:max_j12_org,jz) =  gi_nl2(1:max_j12_org,jz)
            jid_t(1:max_j12_org,1,jz) = lm_gi_nl2(1:max_j12_org,1,jz)
            jid_t(1:max_j12_org,2,jz) = lm_gi_nl2(1:max_j12_org,2,jz)
          end do
!$omp end parallel do
!
          call deallocate_gi_for_nl
          call allocate_gi_for_nl
!
!$omp parallel do
          do j1 = 1, j3-1
            gi_nl2(1:max_j12_org,j1) = di_tmp(1:max_j12_org,j1)
            lm_gi_nl2(1:max_j12_org,1,j1) = jid_t(1:max_j12_org,1,j1)
            lm_gi_nl2(1:max_j12_org,2,j1) = jid_t(1:max_j12_org,2,j1)
          end do
!$omp end parallel do
!
          deallocate(di_tmp, jid_t)
        end if
!
!$omp parallel do private(ist,ied,j1,j2,jz)
        do ip = 1 ,np_smp
          ist = ist_j3_smp(ip-1)+1
          ied = ist_j3_smp(ip)
          num_ki_smp(ip) = ist_ki_smp(ip-1)
          do j1 = ist, ied
            do j2 = 1 ,jmax_gaunt
              if(abs(gi_12(j2,j1)) .ge. 5.0d-16) then
                num_ki_smp(ip) = num_ki_smp(ip) + 1
                jz = num_ki_smp(ip)
                gi_nl2(jz,j3) = gi_12(j2,j1)
                lm_gi_nl2(jz,1,j3) = j1
                lm_gi_nl2(jz,2,j3) = j2
              end if
            end do
          end do
        end do
!$omp end parallel do
      end do
      ntot_gi_nl_lm3 = istack_gi_nl_lm3(jmax_gaunt,1)
!
      end subroutine cal_gaunt_int_nl
!
!  ---------------------------------------------------------------------
!
      subroutine cal_elsasser_int_nl(iflag_db, np_smp, jmax, idx_gl,    &
     &          nidx_3, idx_gl3)
!
      use cal_gaunt_itgs
!
      integer(kind = kint), intent(in) :: iflag_db, np_smp, jmax
      integer(kind = kint), intent(in) :: idx_gl(jmax,3)
      integer(kind = kint), intent(in) :: nidx_3
      integer(kind = kint), intent(in) :: idx_gl3(nidx_3,3)
!
      integer(kind = kint) :: ip, ist, ied, max_j12_org
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
            do j2 = 1 ,jmax_gaunt
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
            do j2 = 1 ,jmax_gaunt
              if(abs(ei_12(j2,j1)) .ge. 5.0d-16)                        &
     &           num_ki_smp(ip) = num_ki_smp(ip) + 1
            end do
          end do
        end do
!$omp end parallel do
!
        do ip = 1 ,np_smp
          num_gi_nl_lm3(j3,2) = num_gi_nl_lm3(j3,2) + num_ki_smp(ip)
          ist_ki_smp(ip) = ist_ki_smp(ip-1) + num_ki_smp(ip)
        end do
        istack_gi_nl_lm3(j3,2) = istack_gi_nl_lm3(j3-1,2)               &
     &                          + num_gi_nl_lm3(j3,2)
!
!
        if( num_gi_nl_lm3(j3,2) .gt. max_j12_ei) then
          max_j12_org = max_j12_ei
          max_j12_ei = num_gi_nl_lm3(j3,2)
!
          allocate( di_tmp(max_j12_org,j3) )
          allocate( jid_t(max_j12_org,2,j3) )
          do jz = 1, j3-1
            di_tmp(1:max_j12_org,jz) =  ei_nl2(1:max_j12_org,jz)
            jid_t(1:max_j12_org,1,jz) = lm_ei_nl2(1:max_j12_org,1,jz)
            jid_t(1:max_j12_org,2,jz) = lm_ei_nl2(1:max_j12_org,2,jz)
          end do
!
          call deallocate_ei_for_nl
          call allocate_ei_for_nl
!
          do j1 = 1, j3-1
            ei_nl2(1:max_j12_org,j1) = di_tmp(1:max_j12_org,j1)
            lm_ei_nl2(1:max_j12_org,1,j1) = jid_t(1:max_j12_org,1,j1)
            lm_ei_nl2(1:max_j12_org,2,j1) = jid_t(1:max_j12_org,2,j1)
          end do
!
          deallocate(di_tmp, jid_t)
        end if
!
!
!$omp parallel do private(ist,ied,j1,j2,jz)
        do ip = 1 ,np_smp
         ist = ist_j3_smp(ip-1)+1
         ied = ist_j3_smp(ip)
         num_ki_smp(ip) = ist_ki_smp(ip-1)
         do j1 = ist, ied
          do j2 = 1 ,jmax_gaunt
            if(abs(ei_12(j2,j1)) .ge. 5.0d-16) then
              num_ki_smp(ip) = num_ki_smp(ip) + 1
              jz = num_ki_smp(ip)
              ei_nl2(jz,j3) = ei_12(j2,j1)
              lm_ei_nl2(jz,1,j3) = j1
              lm_ei_nl2(jz,2,j3) = j2
            end if
          end do
         end do
        end do
!$omp end parallel do
      end do
      ntot_ei_nl_lm3 = istack_gi_nl_lm3(jmax_gaunt,2)
!
      end subroutine cal_elsasser_int_nl
!
!  ---------------------------------------------------------------------
!
      end module cal_int_sph_nonlinear
