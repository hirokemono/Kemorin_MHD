!
!      module link_psf_iso_2_write
!
!      Written by H. Matsui on July, 2006
!
!      subroutine link_psf_outputs
!      subroutine link_iso_outputs
!
      module link_psf_iso_2_write
!
      use m_precision
      use m_geometry_constants
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine link_psf_outputs
!
      use m_control_params_4_psf
      use m_psf_outputs
      use m_multi_ucd_data
!
!
      num_mul_ucd = num_psf
      mul_ucd_header =>       psf_header
      itype_mul_ucd_file =>   itype_psf_file
!
      num_fld_mul_ucd =>    num_psf_output
      num_comp_mul_ucd =>   num_psf_out_comp
      istack_fld_mul_ucd => istack_psf_output
!
      istack_nod_mul_ucd => istack_nod_output_psf
      istack_ele_mul_ucd => istack_ele_output_psf
!
      ntot_fld_mul_ucd_out = num_psf_total_out
      ncomp_mul_ucd => ncomp_psf_output
      fld_name_mul_ucd => name_psf_output
!
      ntot_nod_mul_ucd = ntot_nod_output_psf
      inod_mul_ucd => inod_output_psf
      xx_mul_ucd =>   xx_output_psf
!
      ntot_ele_mul_ucd =   ntot_ele_output_psf
      nnod_4_ele_mul_ucd = num_triangle
      iele_mul_ucd => iele_output_psf
      ie_mul_ucd => ie_output_psf
!
      max_comp_mul_ucd = max_ncomp_psf_out
      dat_mul_ucd => dat_output_psf
!
      end subroutine link_psf_outputs
!
! ----------------------------------------------------------------------
!
      subroutine link_iso_outputs
!
      use m_control_params_4_iso
      use m_iso_outputs
      use m_multi_ucd_data
!
!
      num_mul_ucd = num_iso
      mul_ucd_header =>       iso_header
      itype_mul_ucd_file =>   itype_iso_file
!
      num_fld_mul_ucd => num_iso_output
      num_comp_mul_ucd => num_iso_out_comp
      istack_fld_mul_ucd => istack_iso_output
!
      istack_nod_mul_ucd => istack_nod_output_iso
      istack_ele_mul_ucd => istack_ele_output_iso
!
      ntot_fld_mul_ucd_out = num_iso_total_out
      ncomp_mul_ucd => ncomp_iso_output
      fld_name_mul_ucd => name_iso_output
!
      ntot_nod_mul_ucd = ntot_nod_output_iso
      inod_mul_ucd => inod_output_iso
      xx_mul_ucd =>   xx_output_iso
!
      ntot_ele_mul_ucd =   ntot_ele_output_iso
      nnod_4_ele_mul_ucd = num_triangle
      iele_mul_ucd => iele_output_iso
      ie_mul_ucd => ie_output_iso
!
      max_comp_mul_ucd = max_ncomp_iso_out
      dat_mul_ucd => dat_output_iso
!
      end subroutine link_iso_outputs
!
! ----------------------------------------------------------------------
!
      end module link_psf_iso_2_write
