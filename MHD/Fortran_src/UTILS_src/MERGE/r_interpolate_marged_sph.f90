!>@file   r_interpolate_marged_sph.f90
!!@brief  module r_interpolate_marged_sph
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2014
!
!>@brief Radial interpolation for assemble program
!!
!!@verbatim
!!      subroutine allocate_radial_itp_tbl(nri_new)
!!      subroutine deallocate_radial_itp_tbl(nri_new)
!!      subroutine sph_radial_interpolation_coef                        &
!!     &         (nri_org, r_org, nri_new, r_new)
!!      subroutine itp_rj_merged_phys_from_IO(nnod_org, jmax_org,       &
!!     &          jmax_new, idx_gl_1d_j_org, d_rj_IO)
!!      subroutine extend_potential_magne
!!      subroutine extend_inner_core_temp
!!      subroutine extend_inner_core_light
!!@endverbatim
!!
!!@param   nnod_org  Number of spectr data for original data
!!@param   nri_org   Number of radial grid for original data
!!@param   jmax_org  Number of harmonics modes for original data
!!@param   r_org     Position of radial grid for original data
!!@param   idx_gl_1d_j_org(jmax_org,3)
!!                  List of spherical harmonics modes for original data
!!@param   d_rj_IO   Read harmonics data data
!
      module r_interpolate_marged_sph
!
      use m_precision
!
      implicit none
!
!>      Integer flag if radial grid is same
      integer(kind = kint) :: iflag_same_rgrid =  1
!
!>      Number of radial grids for new spectr data
      integer(kind = kint) :: nri_old2new =  0
!>      Inner radial grid ID for interpolation
      integer(kind = kint), allocatable :: k_old2new_in(:)
!>      Outer radial grid ID for interpolation
      integer(kind = kint), allocatable :: k_old2new_out(:)
!>      Coefficient for Inner grid data for interpolation
      real(kind = kreal), allocatable :: coef_old2new_in(:)
!
!>      Innermost new radial ID within the original domain
      integer(kind = kint) :: kr_inner_domain =  0
!>      Outmost new radial ID within the original domain
      integer(kind = kint) :: kr_outer_domain = 0
!
      private :: extend_inner_core_scalar
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_radial_itp_tbl(nri_new)
!
      integer(kind = kint),  intent(in) :: nri_new
!
!
      nri_old2new = nri_new
      allocate(k_old2new_in(nri_old2new))
      allocate(k_old2new_out(nri_old2new))
      allocate(coef_old2new_in(nri_old2new))
!
      k_old2new_in =    0
      k_old2new_out =   0
      coef_old2new_in = 0.0d0
!
      end subroutine allocate_radial_itp_tbl
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_radial_itp_tbl
!
!
      deallocate(k_old2new_in, k_old2new_out)
      deallocate(coef_old2new_in)
!
      end subroutine deallocate_radial_itp_tbl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sph_radial_interpolation_coef                          &
     &         (nri_org, r_org, nri_new, r_new)
!
      integer(kind = kint), intent(in) :: nri_org, nri_new
      real(kind = kreal), intent(in) :: r_org(nri_org)
      real(kind = kreal), intent(in) :: r_new(nri_org)
!
      integer(kind = kint) :: k, kr_org
      real(kind = kreal) :: r_in, r_out
!
!
      iflag_same_rgrid = 1
      if(nri_org .ne. nri_new) then
        iflag_same_rgrid =  0
      else
        do k = 1, nri_new
          if(abs(r_new(k) - r_org(k)) .gt. 1.0E-12) then
            iflag_same_rgrid = 0
            exit
          end if
        end do
      end if
!
      write(*,*) 'iflag_same_rgrid', iflag_same_rgrid
      if(iflag_same_rgrid .ne. 0) return
!
      call allocate_radial_itp_tbl(nri_new)
!
      do k = 1, nri_new
        if(r_new(k) .lt. r_org(1)) then
          k_old2new_in(k) =    0
          k_old2new_out(k) =   1
          coef_old2new_in(k) = -1.0d0
        else if(r_new(k) .eq. r_org(1)) then
          k_old2new_in(k) =    1
          k_old2new_out(k) =   2
          coef_old2new_in(k) = 1.0d0
        else if(r_new(k) .eq. r_org(nri_org)) then
          k_old2new_in(k) =    nri_org - 1
          k_old2new_out(k) =   nri_org
          coef_old2new_in(k) = 0.0d0
        else if(r_new(k) .gt. r_org(nri_org)) then
          k_old2new_in(k) =    nri_org
          k_old2new_out(k) =   nri_org + 1
          coef_old2new_in(k) = -1.0d0
        else
          do kr_org = 1, nri_org
            r_in =  r_org(kr_org-1)
            r_out = r_org(kr_org  )
            if(r_new(k) .ge. r_in  .and. r_new(k) .lt. r_out) then
              k_old2new_in(k) =  kr_org - 1
              k_old2new_out(k) = kr_org
              coef_old2new_in(k) = (r_out - r_new(k)) / (r_out - r_in)
              exit
            end if
          end do
        end if
      end do
!
      kr_inner_domain = 1
      do k = nri_new, 1, -1
        if(r_new(k) .lt. r_org(1)) then
          kr_inner_domain = k + 1
          exit
        end if
      end do
      kr_outer_domain = nri_new
      do k = 1, nri_new
        if(r_new(k) .gt. r_org(nri_org)) then
          kr_outer_domain = k - 1
          exit
        end if
      end do
!
      write(*,*) 'kr_inner_domain', kr_inner_domain
      write(*,*) 'kr_outer_domain', kr_outer_domain
!      do k = 1, nri_new
!        write(*,'(i5,1pe16.8,2i5,1p3e16.8)') k, r_new(k),             &
!     &         k_old2new_in(k), k_old2new_out(k),                     &
!     &         r_org(k_old2new_in(k)),  r_org(k_old2new_out(k)),      &
!     &         coef_old2new_in(k)
!      end do
!
      end subroutine sph_radial_interpolation_coef
!
! -----------------------------------------------------------------------
!
      subroutine itp_rj_merged_phys_from_IO(nnod_org, jmax_org,         &
     &          jmax_new, idx_gl_1d_j_org, d_rj_IO)
!
      use m_sph_spectr_data
!
      integer(kind = kint), intent(in) :: nnod_org, jmax_org
      integer(kind = kint), intent(in) :: jmax_new
      integer(kind = kint), intent(in) :: idx_gl_1d_j_org(jmax_org,3)
      real(kind = kreal), intent(in) :: d_rj_IO(nnod_org,ntot_phys_rj)
!
      integer(kind = kint) :: nd, j, j_gl, kr
      integer(kind = kint) :: inod_gl, inod_in, inod_out
!
!
!!$omp parallel private(nd)
      do nd = 1, ntot_phys_rj
!!$omp do private(j,j_gl,kr,inod_in,inod_out,inod_gl)
        do j = 1, jmax_org
          j_gl = idx_gl_1d_j_org(j,1)
!
          if(j_gl .lt. jmax_new) then
!
            do kr = kr_inner_domain, kr_outer_domain
              inod_gl = 1 + j_gl + (kr - 1) * jmax_new
              inod_in =  j + (k_old2new_in(kr) - 1) *  jmax_org
              inod_out = j + (k_old2new_out(kr) - 1) * jmax_org
              d_rj(inod_gl,nd)                                          &
     &           = coef_old2new_in(kr) * d_rj_IO(inod_in,nd)            &
     &          + (1.0d0 - coef_old2new_in(kr)) * d_rj_IO(inod_out,nd)
            end do
          end if
        end do
!!$omp end do
      end do
!!$omp end parallel
!
      end subroutine itp_rj_merged_phys_from_IO
!
! -----------------------------------------------------------------------
!
      subroutine extend_potential_magne
!
      use extend_potential_field
!
      use m_phys_labels
      use m_sph_spectr_data
!
      integer(kind = kint) :: is_magne
      integer(kind = kint) :: i
!
!
      is_magne = 0
      do i = 1, num_phys_rj
        if(phys_name_rj(i) .eq. fhd_magne) then
          is_magne = istack_phys_comp_rj(i-1) + 1
          exit
        end if
      end do
      if(is_magne .eq. 0) return
!
      if(kr_outer_domain .lt. nidx_rj(1)) then
        call ext_outside_potential(kr_outer_domain, d_rj(1,is_magne))
      end if
      if(kr_inner_domain .gt. 1) then
        call ext_inside_potential(kr_inner_domain, d_rj(1,is_magne))
      end if
!
      end subroutine extend_potential_magne
!
! -----------------------------------------------------------------------
!
      subroutine extend_inner_core_temp
!
      use m_phys_labels
!
!
      call extend_inner_core_scalar(fhd_temp)
!
      end subroutine extend_inner_core_temp
!
! -----------------------------------------------------------------------
!
      subroutine extend_inner_core_light
!
      use m_phys_labels
!
!
      call extend_inner_core_scalar(fhd_light)
!
      end subroutine extend_inner_core_light
!
! -----------------------------------------------------------------------
!
      subroutine extend_inner_core_scalar(field_name)
!
      use extend_potential_field
      use m_sph_spectr_data
!
      character(len = kchara), intent(in) :: field_name
!
      integer(kind = kint) :: is_field
      integer(kind = kint) :: i
!
!
      is_field = 0
      do i = 1, num_phys_rj
        if(phys_name_rj(i) .eq. field_name) then
          is_field = istack_phys_comp_rj(i-1) + 1
          exit
        end if
      end do
      if(is_field .eq. 0) return
!
      if(kr_inner_domain .gt. 1) then
        call ext_inside_scalar(kr_inner_domain, d_rj(1,is_field))
      end if
!
      end subroutine extend_inner_core_scalar
!
! -----------------------------------------------------------------------
!
      subroutine extend_potential_magne_t(sph_mesh, sph_phys)
!
      use extend_potential_field_t
!
      use m_phys_labels
      use t_sph_spectr_data
      use t_spheric_parameter
!
      type(sph_grids), intent(in) :: sph_mesh
      type(phys_data), intent(inout) :: sph_phys
!
      integer(kind = kint) :: is_magne
      integer(kind = kint) :: i
!
!
      is_magne = 0
      do i = 1, sph_phys%num_phys
        if(sph_phys%phys_name(i) .eq. fhd_magne) then
          is_magne = sph_phys%istack_component(i-1) + 1
          exit
        end if
      end do
      if(is_magne .eq. 0) return
!
      call extend_potential_magne_type(is_magne,                        &
     &      sph_phys%ntot_phys, sph_mesh, sph_phys%d_fld)
!
      end subroutine extend_potential_magne_t
!
! -----------------------------------------------------------------------
!
      subroutine extend_inner_core_scalar_t(field_name, sph_mesh, sph_phys)
!
      use t_sph_spectr_data
      use t_spheric_parameter
!
      character(len = kchara), intent(in) :: field_name
      type(sph_grids), intent(in) :: sph_mesh
      type(phys_data), intent(inout) :: sph_phys
!
!
      integer(kind = kint) :: is_field
      integer(kind = kint) :: i
!
!
      is_field = 0
      do i = 1, sph_phys%num_phys
        if(sph_phys%phys_name(i) .eq. field_name) then
          is_field = sph_phys%istack_component(i-1) + 1
          exit
        end if
      end do
      if(is_field .eq. 0) return
!
      call extend_inner_core_scl_type(is_field, sph_phys%ntot_phys, &
     &    sph_mesh, sph_phys%d_fld)
!
      end subroutine extend_inner_core_scalar_t
!
! -----------------------------------------------------------------------
!
      subroutine extend_potential_magne_type(is_magne,  ntot_phys_rj,   &
     &         sph_mesh, d_rj)
!
      use extend_potential_field_t
!
      use m_phys_labels
      use t_sph_spectr_data
      use t_spheric_parameter
!
      type(sph_grids), intent(in) :: sph_mesh
      integer(kind = kint), intent(in) :: is_magne, ntot_phys_rj
      real(kind= kreal), intent(inout)                                  &
     &                  :: d_rj(sph_mesh%sph_rj%nnod_rj,ntot_phys_rj)
!
!
      write(*,*) 'aho', kr_inner_domain, kr_outer_domain

      if(kr_outer_domain .lt. sph_mesh%sph_rj%nidx_rj(1)) then
        call ext_outside_potential_t(sph_mesh%sph_rj, kr_outer_domain,  &
     &      d_rj(1,is_magne))
      end if
      if(kr_inner_domain .gt. 1) then
        call ext_inside_potential_t(sph_mesh%sph_rj, kr_inner_domain,   &
     &      d_rj(1,is_magne))
      end if
!
      end subroutine extend_potential_magne_type
!
! -----------------------------------------------------------------------
!
      subroutine extend_inner_core_scl_type(is_field, ntot_phys_rj,     &
     &          sph_mesh, d_rj)
!
      use extend_potential_field_t
!
      type(sph_grids), intent(in) :: sph_mesh
      integer(kind = kint), intent(in) :: is_field, ntot_phys_rj
      real(kind= kreal), intent(inout)                                  &
     &                  :: d_rj(sph_mesh%sph_rj%nnod_rj,ntot_phys_rj)
!
!
      if(kr_inner_domain .le. 1) return
        call ext_inside_scalar_t(sph_mesh%sph_rj, kr_inner_domain,      &
     &      d_rj(1,is_field))
!
      end subroutine extend_inner_core_scl_type
!
! -----------------------------------------------------------------------
!
      end module r_interpolate_marged_sph
