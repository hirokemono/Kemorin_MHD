!
!      module m_radial_matrices_sph
!
!     Written by H. Matsui on Apr, 2009
!
!      subroutine allocate_temp_mat_sph
!      subroutine allocate_velo_mat_sph
!      subroutine allocate_magne_mat_sph
!      subroutine allocate_composit_mat_sph
!
!      subroutine deallocate_temp_mat_sph
!      subroutine deallocate_velo_mat_sph
!      subroutine deallocate_magne_mat_sph
!      subroutine deallocate_composit_mat_sph
!
!      subroutine check_velocity_matrices_sph(my_rank)
!      subroutine check_magne_matrices_sph(my_rank)
!      subroutine check_temp_matrices_sph(my_rank)
!      subroutine check_composit_matrix_sph(my_rank)
!
!      subroutine check_radial_5mat_sph(my_rank, nri, jmax, j_sph,      &
!    &           rr, mat)
!
      module m_radial_matrices_sph
!
      use m_precision
!
      implicit none
!
      real(kind = kreal), allocatable :: vsp_evo_mat(:,:,:)
      real(kind = kreal), allocatable :: vsp_evo_lu(:,:,:)
      real(kind = kreal), allocatable :: vsp_evo_det(:,:)
      integer(kind = kint), allocatable :: i_vsp_pivot(:,:)
      real(kind = kreal), allocatable :: vsp_evo_bc(:,:)
      real(kind = kreal), allocatable :: vsp_evo_rhs(:,:)
!
      real(kind = kreal), allocatable :: vp_evo_mat(:,:,:)
      real(kind = kreal), allocatable :: vp_evo_lu(:,:,:)
      real(kind = kreal), allocatable :: vp_evo_det(:,:)
      integer(kind = kint), allocatable :: i_vp_pivot(:,:)
      real(kind = kreal), allocatable :: vp_evo_bc(:,:)
!
      real(kind = kreal), allocatable :: vt_evo_mat(:,:,:)
      real(kind = kreal), allocatable :: vt_evo_lu(:,:,:)
      real(kind = kreal), allocatable :: vt_evo_det(:,:)
      integer(kind = kint), allocatable :: i_vt_pivot(:,:)
      real(kind = kreal), allocatable :: vt_evo_bc(:,:)
!
      real(kind = kreal), allocatable :: wt_evo_mat(:,:,:)
      real(kind = kreal), allocatable :: wt_evo_lu(:,:,:)
      real(kind = kreal), allocatable :: wt_evo_det(:,:)
      integer(kind = kint), allocatable :: i_wt_pivot(:,:)
      real(kind = kreal), allocatable :: wt_evo_bc(:,:)
!
!
      real(kind = kreal), allocatable :: p_poisson_mat(:,:,:)
      real(kind = kreal), allocatable :: p_poisson_lu(:,:,:)
      real(kind = kreal), allocatable :: p_poisson_det(:,:)
      integer(kind = kint), allocatable :: i_p_pivot(:,:)
      real(kind = kreal), allocatable :: p_poisson_bc(:,:)
!
      real(kind = kreal), allocatable :: vs_poisson_mat(:,:,:)
      real(kind = kreal), allocatable :: vs_poisson_lu(:,:,:)
      real(kind = kreal), allocatable :: vs_poisson_det(:,:)
      integer(kind = kint), allocatable :: i_vs_pivot(:,:)
!
!
      real(kind = kreal), allocatable :: bs_evo_mat(:,:,:)
      real(kind = kreal), allocatable :: bs_evo_lu(:,:,:)
      real(kind = kreal), allocatable :: bs_evo_det(:,:)
      integer(kind = kint), allocatable :: i_bs_pivot(:,:)
      real(kind = kreal), allocatable :: bs_evo_bc(:,:)
!
      real(kind = kreal), allocatable :: bt_evo_mat(:,:,:)
      real(kind = kreal), allocatable :: bt_evo_lu(:,:,:)
      real(kind = kreal), allocatable :: bt_evo_det(:,:)
      integer(kind = kint), allocatable :: i_bt_pivot(:,:)
      real(kind = kreal), allocatable :: bt_evo_bc(:,:)
!
!
      real(kind = kreal), allocatable :: temp_evo_mat(:,:,:)
      real(kind = kreal), allocatable :: temp_evo_lu(:,:,:)
      real(kind = kreal), allocatable :: temp_evo_det(:,:)
      integer(kind = kint), allocatable :: i_temp_pivot(:,:)
      real(kind = kreal), allocatable :: temp_evo_bc(:,:)
!
!
      real(kind = kreal), allocatable :: composit_evo_mat(:,:,:)
      real(kind = kreal), allocatable :: composit_evo_lu(:,:,:)
      real(kind = kreal), allocatable :: composit_evo_det(:,:)
      integer(kind = kint), allocatable :: i_composit_pivot(:,:)
      real(kind = kreal), allocatable :: composit_evo_bc(:,:)
!
      private :: check_radial_matrices_sph
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_temp_mat_sph
!
      use m_spheric_parameter
!
      integer(kind = kint) :: nri, jmax
!
      nri =  nidx_rj(1)
      jmax = nidx_rj(2)
!
      allocate( temp_evo_mat(3,nri,jmax) )
      allocate( temp_evo_lu(5,nri,jmax) )
      allocate( temp_evo_det(nri,jmax) )
      allocate( temp_evo_bc(2,jmax) )
      allocate( i_temp_pivot(nri,jmax) )
!
      temp_evo_mat =  0.0d0
      temp_evo_lu =   0.0d0
      temp_evo_det =   0.0d0
      temp_evo_bc = 0.0d0
      i_temp_pivot =   0
      temp_evo_mat(2,1:nri,1:jmax) = 1.0d0
!
      end subroutine allocate_temp_mat_sph
!
! -----------------------------------------------------------------------
!
      subroutine allocate_velo_mat_sph
!
      use m_spheric_parameter
!
      integer(kind = kint) :: nri, jmax
!
      nri = nidx_rj(1)
      jmax = nidx_rj(2)
!
!
      allocate( vs_poisson_mat(3,nri,jmax) )
      allocate( vs_poisson_lu(5,nri,jmax) )
      allocate( vs_poisson_det(nri,jmax) )
      allocate( i_vs_pivot(nri,jmax) )
!
      allocate( vsp_evo_mat(7,2*nri,jmax) )
      allocate( vsp_evo_lu(13,2*nri,jmax) )
      allocate( vsp_evo_det(2*nri,jmax) )
      allocate( vsp_evo_bc(2,2*jmax) )
      allocate( i_vsp_pivot(2*nri,jmax) )
      allocate( vsp_evo_rhs(jmax,2*nri) )
!
      allocate( vp_evo_mat(5,nri,jmax) )
      allocate( vp_evo_lu(9,nri,jmax) )
      allocate( vp_evo_det(nri,jmax) )
      allocate( vp_evo_bc(2,jmax) )
      allocate( i_vp_pivot(nri,jmax) )
!
      allocate( vt_evo_mat(3,nri,jmax) )
      allocate( vt_evo_lu(5,nri,jmax) )
      allocate( vt_evo_det(nri,jmax) )
      allocate( vt_evo_bc(2,jmax) )
      allocate( i_vt_pivot(nri,jmax) )
!
      allocate( wt_evo_mat(3,nri,jmax) )
      allocate( wt_evo_lu(5,nri,jmax) )
      allocate( wt_evo_det(nri,jmax) )
      allocate( wt_evo_bc(2,jmax) )
      allocate( i_wt_pivot(nri,jmax) )
!
      allocate( p_poisson_mat(3,nri,jmax) )
      allocate( p_poisson_lu(5,nri,jmax) )
      allocate( p_poisson_det(nri,jmax) )
      allocate( p_poisson_bc(2,jmax) )
      allocate( i_p_pivot(nri,jmax) )
!
      vsp_evo_mat =   0.0d0
      vsp_evo_lu =    0.0d0
      vsp_evo_det =   0.0d0
      vsp_evo_bc =    0.0d0
      vsp_evo_rhs =   0.0d0
      i_vsp_pivot =   0
!
      vp_evo_mat =   0.0d0
      vp_evo_lu =    0.0d0
      vp_evo_det =   0.0d0
      vp_evo_bc =    0.0d0
      i_vp_pivot =   0
!
      vt_evo_mat =   0.0d0
      vt_evo_lu =    0.0d0
      vt_evo_det =   0.0d0
      vt_evo_bc =   0.0d0
      i_vt_pivot =   0
!
      wt_evo_mat =   0.0d0
      wt_evo_lu =    0.0d0
      wt_evo_det =   0.0d0
      wt_evo_bc =   0.0d0
      i_wt_pivot =   0
!
      vs_poisson_mat =   0.0d0
      vs_poisson_lu =    0.0d0
      vs_poisson_det =   0.0d0
      i_vs_pivot =   0
!
      p_poisson_mat = 0.0d0
      p_poisson_lu =  0.0d0
      p_poisson_det = 0.0d0
      p_poisson_bc =  0.0d0
      i_p_pivot =     0
!
      vsp_evo_mat(4,1:2*nri,1:jmax) = 1.0d0
      vp_evo_mat(3,1:nri,1:jmax) = 1.0d0
      vt_evo_mat(2,1:nri,1:jmax) = 1.0d0
      wt_evo_mat(2,1:nri,1:jmax) = 1.0d0
!
      if(nlayer_ICB .gt. 1) then
        p_poisson_mat(2,1:nlayer_ICB-1,1:jmax) =  1.0d0
        vs_poisson_mat(2,1:nlayer_ICB-1,1:jmax) = 1.0d0
      end if
!
      if(nlayer_CMB .lt. nri) then
        p_poisson_mat(2,nlayer_CMB+1:nri,1:jmax) =  1.0d0
        vs_poisson_mat(2,nlayer_CMB+1:nri,1:jmax) = 1.0d0
      end if
!
      end subroutine allocate_velo_mat_sph
!
! -----------------------------------------------------------------------
!
      subroutine allocate_magne_mat_sph
!
      use m_spheric_parameter
!
      integer(kind = kint) :: nri, jmax
!
!
      nri = nidx_rj(1)
      jmax = nidx_rj(2)
!
      allocate( bs_evo_mat(3,nri,jmax) )
      allocate( bs_evo_lu(5,nri,jmax) )
      allocate( bs_evo_det(nri,jmax) )
      allocate( bs_evo_bc(2,jmax) )
      allocate( i_bs_pivot(nri,jmax) )
!
      allocate( bt_evo_mat(3,nri,jmax) )
      allocate( bt_evo_lu(5,nri,jmax) )
      allocate( bt_evo_det(nri,jmax) )
      allocate( bt_evo_bc(2,jmax) )
      allocate( i_bt_pivot(nri,jmax) )
!
      bs_evo_mat =  0.0d0
      bs_evo_lu =   0.0d0
      bs_evo_det =  0.0d0
      bs_evo_bc =   0.0d0
      i_bs_pivot =  0
!
      bt_evo_mat =  0.0d0
      bt_evo_lu =   0.0d0
      bt_evo_det =  0.0d0
      bt_evo_bc =   0.0d0
      i_bt_pivot =  0
!
      bs_evo_mat(2,1:nri,1:jmax) = 1.0d0
      bt_evo_mat(2,1:nri,1:jmax) = 1.0d0
!
      end subroutine allocate_magne_mat_sph
!
! -----------------------------------------------------------------------
!
      subroutine allocate_composit_mat_sph
!
      use m_spheric_parameter
!
      integer(kind = kint) :: nri, jmax
!
      nri =  nidx_rj(1)
      jmax = nidx_rj(2)
!
      allocate( composit_evo_mat(3,nri,jmax) )
      allocate( composit_evo_lu(5,nri,jmax) )
      allocate( composit_evo_det(nri,jmax) )
      allocate( composit_evo_bc(2,jmax) )
      allocate( i_composit_pivot(nri,jmax) )
!
      composit_evo_mat =  0.0d0
      composit_evo_lu =   0.0d0
      composit_evo_det =  0.0d0
      composit_evo_bc   = 0.0d0
      i_composit_pivot =      0
      composit_evo_mat(2,1:nri,1:jmax) = 1.0d0
!
      end subroutine allocate_composit_mat_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_temp_mat_sph
!
!
      deallocate( temp_evo_mat, temp_evo_lu )
      deallocate( temp_evo_det, i_temp_pivot)
      deallocate( temp_evo_bc)
!
      end subroutine deallocate_temp_mat_sph
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_velo_mat_sph
!
!
      deallocate( vs_poisson_mat, vs_poisson_lu )
      deallocate( vs_poisson_det, i_vs_pivot )
!
      deallocate( vsp_evo_mat, vsp_evo_lu )
      deallocate( vsp_evo_det, i_vsp_pivot )
      deallocate( vsp_evo_bc,  vsp_evo_rhs )
!
      deallocate( vp_evo_mat, vp_evo_lu )
      deallocate( vp_evo_det, i_vp_pivot )
      deallocate( vp_evo_bc )
!
      deallocate( vt_evo_mat, vt_evo_lu )
      deallocate( vt_evo_det, i_vt_pivot )
      deallocate( vt_evo_bc )
!
      deallocate( wt_evo_mat, wt_evo_lu )
      deallocate( wt_evo_det, i_wt_pivot )
      deallocate( wt_evo_bc )
!
      deallocate( p_poisson_mat, p_poisson_lu )
      deallocate( p_poisson_det, i_p_pivot )
      deallocate( p_poisson_bc )
!
      end subroutine deallocate_velo_mat_sph
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_magne_mat_sph
!
!
      deallocate( bs_evo_mat, bs_evo_lu )
      deallocate( bs_evo_det, i_bs_pivot )
!
      deallocate( bt_evo_mat, bt_evo_lu )
      deallocate( bt_evo_det, i_bt_pivot )
      deallocate( bt_evo_bc )
!
      end subroutine deallocate_magne_mat_sph
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_composit_mat_sph
!
!
      deallocate( composit_evo_mat, composit_evo_lu )
      deallocate( composit_evo_det, i_composit_pivot )
      deallocate( composit_evo_bc )
!
      end subroutine deallocate_composit_mat_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_velocity_matrices_sph(my_rank)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: my_rank
!
!
!      write(50+my_rank,'(a)') 'poisson matrix for poloidal velocity'
!      call check_radial_matrices_sph(my_rank, nidx_rj(1), nidx_rj(2),  &
!     &    idx_gl_1d_rj_j, radius_1d_rj_r, vs_poisson_mat)
!
      write(50+my_rank,'(a)') 'crank matrix for poloidal velocity'
!
      write(50+my_rank,'(a)') 'crank matrix for toroidal velocity'
      call check_radial_matrices_sph(my_rank, nidx_rj(1), nidx_rj(2),   &
     &    idx_gl_1d_rj_j, radius_1d_rj_r, vt_evo_mat)
!
!      write(50+my_rank,'(a)') 'crank matrix for toroidal vorticity'
!      call check_radial_matrices_sph(my_rank, nidx_rj(1), nidx_rj(2),  &
!     &    idx_gl_1d_rj_j, radius_1d_rj_r, wt_evo_mat)
!
      write(50+my_rank,'(a)') 'poisson matrix for pressure'
      call check_radial_matrices_sph(my_rank, nidx_rj(1), nidx_rj(2),   &
     &    idx_gl_1d_rj_j, radius_1d_rj_r, p_poisson_mat)
!
      end subroutine check_velocity_matrices_sph
!
! -----------------------------------------------------------------------
!
      subroutine check_vorticity_matrices_sph(my_rank)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      write(50+my_rank,'(a)') 'poisson matrix for poloidal velocity'
      call check_radial_matrices_sph(my_rank, nidx_rj(1), nidx_rj(2),   &
     &    idx_gl_1d_rj_j, radius_1d_rj_r, vs_poisson_mat)
!
      write(50+my_rank,'(a)') 'crank matrix for poloidal velocity'
      call check_radial_5mat_sph(my_rank, nidx_rj(1), nidx_rj(2),       &
     &    idx_gl_1d_rj_j, radius_1d_rj_r, vp_evo_mat)
!
      write(50+my_rank,'(a)') 'crank matrix for toroidal velocity'
      call check_radial_matrices_sph(my_rank, nidx_rj(1), nidx_rj(2),   &
     &    idx_gl_1d_rj_j, radius_1d_rj_r, vt_evo_mat)
!
      write(50+my_rank,'(a)') 'crank matrix for toroidal vorticity'
      call check_radial_matrices_sph(my_rank, nidx_rj(1), nidx_rj(2),   &
     &    idx_gl_1d_rj_j, radius_1d_rj_r, wt_evo_mat)
!
      write(50+my_rank,'(a)') 'poisson matrix for pressure'
      call check_radial_matrices_sph(my_rank, nidx_rj(1), nidx_rj(2),   &
     &    idx_gl_1d_rj_j, radius_1d_rj_r, p_poisson_mat)
!
      write(50+my_rank,'(a)') 'crank matrix for poloidal velocity'
      call check_radial_5mat_sph(my_rank, nidx_rj(1), nidx_rj(2),      &
     &    idx_gl_1d_rj_j, radius_1d_rj_r, vp_evo_mat)
!
      end subroutine check_vorticity_matrices_sph
!
! -----------------------------------------------------------------------
!
      subroutine check_magne_matrices_sph(my_rank)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      write(50+my_rank,'(a)') 'crank matrix for poloidal magne'
      call check_radial_matrices_sph(my_rank, nidx_rj(1), nidx_rj(2),   &
     &    idx_gl_1d_rj_j, radius_1d_rj_r, bs_evo_mat)
!
      write(50+my_rank,'(a)') 'crank matrix for toroidal magne'
      call check_radial_matrices_sph(my_rank, nidx_rj(1), nidx_rj(2),   &
     &    idx_gl_1d_rj_j, radius_1d_rj_r, bt_evo_mat)
!
      end subroutine check_magne_matrices_sph
!
! -----------------------------------------------------------------------
!
      subroutine check_temp_matrices_sph(my_rank)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      write(50+my_rank,'(a)') 'crank matrix for temperature'
      call check_radial_matrices_sph(my_rank, nidx_rj(1), nidx_rj(2),   &
     &    idx_gl_1d_rj_j, radius_1d_rj_r, temp_evo_mat)
!
      end subroutine check_temp_matrices_sph
!
! -----------------------------------------------------------------------
!
      subroutine check_composit_matrix_sph(my_rank)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      write(50+my_rank,'(a)') 'crank matrix for dummy scalar'
      call check_radial_matrices_sph(my_rank, nidx_rj(1), nidx_rj(2),   &
     &    idx_gl_1d_rj_j, radius_1d_rj_r, composit_evo_mat)
!
      end subroutine check_composit_matrix_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_radial_matrices_sph(my_rank, nri, jmax, j_sph,   &
     &           rr, mat)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: nri, jmax
      integer(kind = kint), intent(in) :: j_sph(jmax,3)
      real(kind = kreal), intent(in) :: rr(nri)
      real(kind = kreal), intent(in) :: mat(3,nri,jmax)
!
      integer(kind = kint) :: i, j, k
!
!
      write(50+my_rank,'(a)') 'j, l, m, k, r, a(k,k-1), a(k,k), a(k,k+1)'
      do j = 1, jmax
!
        i = 1 + (j-1)*nri
        write(50+my_rank,'(5i6,1p4E25.15e3)') j, j_sph(j,1:3), 1,       &
     &              rr(1), -1.0d30, mat(2,1,j), mat(1,2,j)
        do k = 2, nri-1
          i = k + (j-1)*nri
          write(50+my_rank,'(5i6,1p4E25.15e3)') j, j_sph(j,1:3), k,     &
     &              rr(k), mat(3,k-1,j), mat(2,k,j), mat(1,k+1,j)
        end do
        i = nri + (j-1)*nri
        write(50+my_rank,'(5i6,1p4E25.15e3)') j, j_sph(j,1:3), nri,     &
     &              rr(nri), mat(3,nri-1,j), mat(2,nri,j), 1.0d30
      end do
!
      end subroutine check_radial_matrices_sph
!
! -----------------------------------------------------------------------
!
      subroutine check_radial_5mat_sph(my_rank, nri, jmax, j_sph,       &
     &           rr, mat)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: nri, jmax
      integer(kind = kint), intent(in) :: j_sph(jmax,3)
      real(kind = kreal), intent(in) :: rr(nri)
      real(kind = kreal), intent(in) :: mat(5,nri,jmax)
!
      integer(kind = kint) :: i, j, k, l
!
!
      write(50+my_rank,'(a)') 'j, l, m, k, r, a(k,k-1), a(k,k), a(k,k+1)'
      do j = 1, jmax
!
        i = 1 + (j-1)*nri
        write(50+my_rank,'(5i6,1p6E25.15e3)') j, j_sph(j,1:3), 1,       &
     &              rr(1), -1.0d30, -1.0d30, (mat(3-l,1+l,j),l=0,2)
        i = 2 + (j-1)*nri
        write(50+my_rank,'(5i6,1p6E25.15e3)') j, j_sph(j,1:3), 2,       &
     &              rr(2), -1.0d30, (mat(3-l,2+l,j),l=-1,2)
        do k = 3, nri-2
          i = k + (j-1)*nri
          write(50+my_rank,'(5i6,1p6E25.15e3)') j, j_sph(j,1:3), k,     &
     &              rr(k), (mat(3-l,k+l,j),l=-2,2)
        end do
        i = (nri-1) + (j-1)*nri
        write(50+my_rank,'(5i6,1p6E25.15e3)') j, j_sph(j,1:3), (nri-1), &
     &              rr(nri-1), (mat(3-l,nri-1+l,j),l=-2,1), 1.0d30
        i = nri + (j-1)*nri
        write(50+my_rank,'(5i6,1p6E25.15e3)') j, j_sph(j,1:3), nri,     &
     &              rr(nri), (mat(3-l,nri+l,j),l=-2,0), 1.0d30, 1.0d30
      end do
!
      end subroutine check_radial_5mat_sph
!
! -----------------------------------------------------------------------
!
      end module m_radial_matrices_sph
