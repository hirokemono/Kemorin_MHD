!set_fields_for_psf.f90
!      module set_fields_for_psf
!
!      Written by H. Matsui on June, 2006
!
!      subroutine set_field_4_psf(numnod, numedge, nnod_4_edge, ie_edge,&
!     &          num_phys, ntot_phys, istack_ncomp,  d_nod)
!      subroutine set_field_4_iso(numnod, numedge, nnod_4_edge, ie_edge,&
!     &  num_phys, ntot_phys, istack_ncomp, d_nod)
!
      module set_fields_for_psf
!
      use m_precision
!
      use m_machine_parameter
!
      implicit none
!
      private :: set_field_on_psf
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_field_4_psf(numnod, numedge, nnod_4_edge, ie_edge, &
     &          num_phys, ntot_phys, istack_ncomp,  d_nod)
!
      use m_control_params_4_psf
      use m_psf_data
!
      integer(kind = kint), intent(in) :: numnod, numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
!
      integer(kind = kint), intent(in) :: num_phys, ntot_phys
      integer(kind = kint), intent(in) :: istack_ncomp(0:num_phys)
      real(kind = kreal), intent(in)  :: d_nod(numnod,ntot_phys)
!
      integer(kind = kint) :: i, ist_smp, ist_field
!
      do i = 1, num_psf
!
        ist_smp = (i-1)*np_smp
        ist_field = istack_psf_output(i-1) + 1
        call set_field_on_psf(numnod, numedge, nnod_4_edge, ie_edge,    &
     &      psf_pat%nnod_psf_tot, istack_nod_psf_smp(ist_smp),          &
     &      psf_pat%xyz_psf, psf_pat%rr, psf_pat%ar, psf_pat%ss,        &
     &      psf_pat%as, num_psf_output(i), max_ncomp_psf_out,           &
     &      id_psf_output(ist_field), ncomp_psf_output(ist_field),      &
     &      ncomp_psf_org(ist_field), icomp_psf_output(ist_field),      &
     &      num_phys, ntot_phys, istack_ncomp,                          &
     &      d_nod, psf_pat%dat_psf, psf_pat%tmp_psf, psf_list(i))
!
      end do
!
      end subroutine set_field_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_field_4_iso(numnod, numedge, nnod_4_edge, ie_edge, &
     &  num_phys, ntot_phys, istack_ncomp, d_nod)
!
      use m_control_params_4_iso
      use m_iso_data
!
      use set_nodal_field_for_psf
!
      integer(kind = kint), intent(in) :: numnod, numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
!
      integer(kind = kint), intent(in) :: num_phys, ntot_phys
      integer(kind = kint), intent(in) :: istack_ncomp(0:num_phys)
      real(kind = kreal), intent(in)  :: d_nod(numnod,ntot_phys)
!
      integer(kind = kint) :: i, ist_smp, ist_field
!
      do i = 1, num_iso
!
        ist_field = istack_iso_output(i-1) + 1
        ist_smp = (i-1)*np_smp
!
        if (id_iso_output(ist_field) .eq. iflag_constant_iso) then
          call set_const_on_psf(iso_pat%nnod_psf_tot,                   &
     &        istack_nod_iso_smp(ist_smp), result_value_iso(i),         &
     &        iso_pat%dat_psf, iso_list(i))
!
        else
          call set_field_on_psf(numnod, numedge, nnod_4_edge, ie_edge,  &
     &      iso_pat%nnod_psf_tot, istack_nod_iso_smp(ist_smp),          &
     &      iso_pat%xyz_psf, iso_pat%rr, iso_pat%ar, iso_pat%ss,        &
     &      iso_pat%as, num_iso_output(i), max_ncomp_iso_out,           &
     &      id_iso_output(ist_field), ncomp_iso_output(ist_field),      &
     &      ncomp_iso_org(ist_field), icomp_iso_output(ist_field),      &
     &      num_phys, ntot_phys, istack_ncomp,                          &
     &      d_nod, iso_pat%dat_psf, iso_pat%tmp_psf, iso_list(i))
        end if
!
      end do
!
      end subroutine set_field_4_iso
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_field_on_psf(numnod, numedge, nnod_4_edge,         &
     &      ie_edge, nnod_patch, istack_n_smp, xyz_psf, rr_psf, ar_psf, &
     &      ss_psf, as_psf, nfield_psf, max_ncomp_psf, ifield_psf,      &
     &      ncomp_psf, ncomp_org, icomp_psf, num_phys, ntot_phys,       &
     &      istack_ncomp, d_nod, dat_psf, dat_tmp, psf_list)
!
      use t_psf_geometry_list
      use m_geometry_constants
!
      use set_components_flags
      use set_nodal_field_for_psf
      use convert_components_4_viz
!
      integer(kind = kint), intent(in) :: numnod, numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
!
      integer(kind = kint), intent(in) :: nnod_patch
      integer(kind = kint), intent(in) :: istack_n_smp(0:np_smp)
      real(kind = kreal), intent(in) :: xyz_psf(nnod_patch,3)
      real(kind = kreal), intent(in) :: rr_psf(nnod_patch)
      real(kind = kreal), intent(in) :: ar_psf(nnod_patch)
      real(kind = kreal), intent(in) :: ss_psf(nnod_patch)
      real(kind = kreal), intent(in) :: as_psf(nnod_patch)
      integer(kind = kint), intent(in) :: nfield_psf, max_ncomp_psf
      integer(kind = kint), intent(in) :: ifield_psf(nfield_psf)
      integer(kind = kint), intent(in) :: ncomp_psf(nfield_psf)
      integer(kind = kint), intent(in) :: ncomp_org(nfield_psf)
      integer(kind = kint), intent(in) :: icomp_psf(nfield_psf)
!
      type(sectiong_list), intent(in) :: psf_list
!
      integer(kind = kint), intent(in) :: num_phys, ntot_phys
      integer(kind = kint), intent(in) :: istack_ncomp(0:num_phys)
      real(kind = kreal), intent(in)  :: d_nod(numnod,ntot_phys)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: dat_psf(nnod_patch,max_ncomp_psf)
!
      real(kind = kreal), intent(inout) :: dat_tmp(nnod_patch,6)
!
      integer(kind = kint) :: i, icou
!
!
      icou = 0
      do i = 1, nfield_psf
!
!        write(*,*) 'i', i, ifield_psf(i), ncomp_org(i), icomp_psf(i)
!
        call set_field_on_psf_xyz(numnod, numedge, nnod_4_edge,         &
     &          ie_edge, nnod_patch, istack_n_smp,                      &
     &          num_phys, ntot_phys, istack_ncomp, d_nod,               &
     &          ifield_psf(i), ncomp_org(i), dat_tmp, psf_list)
!
        call convert_comps_4_viz(nnod_patch, istack_n_smp,              &
     &      xyz_psf, rr_psf, ar_psf, ss_psf, as_psf,                    &
     &      ncomp_psf(i), ncomp_org(i), icomp_psf(i),                   &
     &      dat_tmp(1,1), dat_psf(1,icou+1))
        icou = icou + ncomp_psf(i)
      end do
!
!
      end subroutine set_field_on_psf
!
!  ---------------------------------------------------------------------
!
      end module set_fields_for_psf
