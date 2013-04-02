!
!      module m_work_4_sph_trans
!
!     Written by H. Matsui on July, 2007
!
!      subroutine resize_work_4_sph_trans
!
!      subroutine allocate_work_4_sph_trans
!      subroutine deallocate_work_4_sph_trans
!
!      subroutine allocate_work_4_zonal_fft
!      subroutine deallocate_work_4_zonal_fft
!
!      subroutine allocate_wk_nod_data_to_sph
!      subroutine deallocate_wk_nod_data_to_sph
!
!      subroutine check_vr_rtp(my_rank, nb)
!      subroutine check_vr_rtm(my_rank, nb)
!      subroutine check_sp_rlm(my_rank, nb)
!
!   compressed array
!   input /outpt arrays
!
!      radial component:      vr_rtp(3*i_rtp-2)
!      elevetional component: vr_rtp(3*i_rtp-1)
!      azimuthal component:   vr_rtp(2*i_rtp  )
!
!      Poloidal component:          sp_rj(3*i_rj-2)
!      diff. of Poloidal component: sp_rj(3*i_rj-1)
!      Toroidal component:          sp_rj(3*i_rj  )
!
!  transform for scalar
!   input /outpt arrays
!
!      field: vr_rtp(i_rtp)
!      spectr: sp_rj(i_rj)
!
      module m_work_4_sph_trans
!
      use m_precision
!
      implicit none
!
!
      integer(kind = kint), parameter :: iflag_lag_undefined = -1
      integer(kind = kint), parameter :: iflag_lag_largest_loop = 1
      integer(kind = kint), parameter :: iflag_lag_krloop_inner = 2
      integer(kind = kint), parameter :: iflag_lag_krloop_outer = 3
      integer(kind = kint)                                              &
     &              :: id_lagendre_transfer = iflag_lag_undefined
!
      integer(kind = kint) :: nb_sph_trans
!
      real(kind = kreal), allocatable :: d_nod_rtp(:,:)
!
      real(kind = kreal), allocatable :: sp_rj(:)
      real(kind = kreal), allocatable :: vr_rtp(:)
!
      real(kind = kreal), allocatable :: sp_rlm(:)
      real(kind = kreal), allocatable :: vr_rtm(:)
!
!      field: vr_rtm(i)... vr_rtm(i_comp,i_fld,l_rtm,k_rtm,m_rtm)
!       size: vr_rtm_1(3,nb,nidx_rtm(2),nidx_rtm(1),nidx_rtm(3))
!      spectr: sp_rlm(j)...sp_rlm(i_comp,i_fld,j_rlm,k_rtm)
!        size: sp_rlm(3,nb,nidx_rlm(2),nidx_rtm(1))
!
      real(kind = kreal), allocatable :: sp_rlm_1(:,:,:)
      real(kind = kreal), allocatable :: vr_rtm_1(:,:,:)
!      real(kind = kreal), allocatable :: vr_rtm_1n(:,:,:)
!      field: vr_rtm_1(i)... vr_rtm_1(i_fld,k_rtm,l_rtm,m_rtm,i_comp)
!       size: vr_rtm_1(nb,nidx_rtm(1),nidx_rtm(2),nidx_rtm(3),3)
!      spectr: sp_rlm_1(j)...sp_rlm_1(i_fld,k_rtm,j_rlm,i_comp)
!        size: sp_rlm_1(nb,nidx_rtm(1),nidx_rlm(2),3)
!
      real(kind = kreal), allocatable :: sp_rlm_2(:,:,:)
      real(kind = kreal), allocatable :: vr_rtm_2(:,:,:,:)
!      field: vr_rtm_2(i,i_comp).vr_rtm_2(l_rtm,m_rtm,k_rtm,i_fld,i_comp)
!       size: vr_rtm_2(nidx_rtm(2),nidx_rtm(3),nidx_rtm(1),nb,3)
!      spectr: sp_rlm_2(j,i_comp)...sp_rlm_2(j_rlm,k_rtm,i_fld,i_comp)
!        size: sp_rlm_2(nidx_rlm(2),nidx_rtm(1),nb,3)
!
!
      integer(kind = kint), allocatable :: jdx_p_rlm_rtm(:)
      integer(kind = kint), allocatable :: jdx_n_rlm_rtm(:)
      integer(kind = kint), allocatable :: mdx_p_rlm_rtm(:)
      integer(kind = kint), allocatable :: mdx_n_rlm_rtm(:)
      real(kind = kreal), allocatable :: asin_theta_1d_rtm(:)
!
      integer(kind = kint), allocatable :: lstack_rlm(:)
!
      integer(kind = kint), private :: iflag_sph_trans = -1
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine resize_work_4_sph_trans
!
      if (nb_sph_trans .gt. iflag_sph_trans) then
        call deallocate_work_4_sph_trans
        call allocate_work_4_sph_trans
        return
      end if
!
      if (iflag_sph_trans .le. 0)  call allocate_work_4_sph_trans
!
      end subroutine resize_work_4_sph_trans
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine allocate_work_4_sph_trans
!
      use m_spheric_parameter
!
      integer(kind = kint) :: num1, num2
!
      allocate(lstack_rlm(0:nidx_rtm(3)))
!
      allocate(mdx_p_rlm_rtm(nidx_rlm(2)))
      allocate(mdx_n_rlm_rtm(nidx_rlm(2)))
      allocate(asin_theta_1d_rtm(nidx_rtm(2)))
!
      allocate(sp_rlm(3*nb_sph_trans*nnod_rlm))
      num1 = nb_sph_trans*nidx_rlm(1)
      allocate(sp_rlm_1(num1,nidx_rlm(2),3))
      allocate(sp_rlm_2(nidx_rlm(2),num1,3))
!
      allocate(vr_rtp(3*nb_sph_trans*nnod_rtp))
!
      allocate(sp_rj(3*nb_sph_trans*nnod_rj))
!
      allocate(vr_rtm(3*nb_sph_trans*nnod_rtm))
      num1 = nb_sph_trans*nidx_rlm(1)
      num2 = nidx_rtm(2)*nidx_rtm(3)
      allocate(vr_rtm_1(num1,num2,3))
!      allocate(vr_rtm_1n(num1,num2,3))
      allocate(vr_rtm_2(nidx_rtm(2),nidx_rtm(3),num1,3))
!
!
      lstack_rlm = 0
      mdx_p_rlm_rtm = 0
      mdx_n_rlm_rtm = 0
      asin_theta_1d_rtm = 0.0d0
!
      vr_rtp = 0.0d0
!
      sp_rj = 0.0d0
      sp_rlm = 0.0d0
      vr_rtm = 0.0d0
!
      sp_rlm_1 = 0.0d0
      vr_rtm_1 = 0.0d0
      sp_rlm_2 = 0.0d0
      vr_rtm_2 = 0.0d0
!
!      vr_rtm_1n = 0.0d0
!
      iflag_sph_trans = nb_sph_trans
!
      end subroutine allocate_work_4_sph_trans
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_work_4_sph_trans
!
      deallocate(lstack_rlm)
      deallocate(mdx_p_rlm_rtm, mdx_n_rlm_rtm)
      deallocate(asin_theta_1d_rtm)
!
      deallocate(vr_rtp)
!
      deallocate(sp_rj)
      deallocate(sp_rlm, sp_rlm_1, sp_rlm_2)
      deallocate(vr_rtm, vr_rtm_1, vr_rtm_2)
!
      iflag_sph_trans = 0
!
      end subroutine deallocate_work_4_sph_trans
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine allocate_work_4_zonal_fft
!
      use m_spheric_parameter
!
      allocate(vr_rtp(3*nb_sph_trans*nnod_rtp))
      vr_rtp = 0.0d0
!
      iflag_sph_trans = nb_sph_trans
!
      end subroutine allocate_work_4_zonal_fft
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_work_4_zonal_fft
!
      deallocate(vr_rtp)
      iflag_sph_trans = 0
!
      end subroutine deallocate_work_4_zonal_fft
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine allocate_wk_nod_data_to_sph
!
      use m_spheric_parameter
!
      allocate( d_nod_rtp(nnod_rtp_pole,6) )
      d_nod_rtp = 0.0d0
!
      end subroutine allocate_wk_nod_data_to_sph
!
! -------------------------------------------------------------------
!
      subroutine deallocate_wk_nod_data_to_sph
!
      deallocate( d_nod_rtp )
!
      end subroutine deallocate_wk_nod_data_to_sph
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine check_vr_rtp(my_rank, nb)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: my_rank, nb
      integer(kind = kint) :: inod, ist, ied
!
      write(50+my_rank,*) 'vr_rtp', nb
      do inod = 1, nnod_rtp
        ist = (inod-1) * nb + 1
        ied = (inod-1) * nb + nb
        write(50+my_rank,'(4i10,1p200e20.12)') inod,                    &
     &        idx_global_rtp(inod,1:3), vr_rtp(ist:ied)
      end do
!
      end subroutine check_vr_rtp
!
! ----------------------------------------------------------------------
!
      subroutine check_vr_rtm(my_rank, nb)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: my_rank, nb
      integer(kind = kint) :: inod, ist, ied
!
      write(50+my_rank,*) 'vr_rtm', nb
      do inod = 1, nnod_rtm
        ist = (inod-1) * nb + 1
        ied = (inod-1) * nb + nb
        write(50+my_rank,'(4i10,1p200e20.12)') inod,                    &
     &        idx_global_rtm(inod,1:3), vr_rtm(ist:ied)
      end do
!
      end subroutine check_vr_rtm
!
! ----------------------------------------------------------------------
!
      subroutine check_sp_rlm(my_rank, nb)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: my_rank, nb
      integer(kind = kint) :: inod, ist, ied
!
      write(50+my_rank,*) 'sp_rlm', nb
      do inod = 1, nnod_rlm
        ist = (inod-1) * nb + 1
        ied = (inod-1) * nb + nb
        write(50+my_rank,'(3i10,1p200e20.12)') inod,                    &
     &        idx_global_rlm(inod,1:2), sp_rlm(ist:ied)
      end do
!
      end subroutine check_sp_rlm
!
! ----------------------------------------------------------------------
!
      subroutine check_sp_rj(my_rank, nb)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: my_rank, nb
      integer(kind = kint) :: inod, ist, ied
!
      write(50+my_rank,*) 'sp_rj', nb
      do inod = 1, nnod_rj
        ist = (inod-1) * nb + 1
        ied = (inod-1) * nb + nb
        write(50+my_rank,'(3i10,1p200e20.12)') inod,                    &
     &        idx_global_rj(inod,1:2), sp_rj(ist:ied)
      end do
!
      end subroutine check_sp_rj
!
! ----------------------------------------------------------------------
!
      end module m_work_4_sph_trans
