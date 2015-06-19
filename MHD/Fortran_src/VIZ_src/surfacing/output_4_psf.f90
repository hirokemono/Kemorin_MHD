!>@file   output_4_psf.f90
!!@brief  module output_4_psf
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!!@n    modified in July, 2014
!
!>@brief Output routines for surfacing module
!!
!!@verbatim
!!      subroutine output_section_mesh(psf_header, itype_psf_file,      &
!!     &         psf_mesh, psf_out, psf_out_m)
!!      subroutine output_section_data                                  &
!!     &         (istep_psf, psf_mesh, psf_out, psf_out_m)
!!
!!      subroutine output_isosurface                                    &
!!     &         (iso_header, itype_iso_file, istep_iso, iso_mesh)
!!@endverbatim
!
      module output_4_psf
!
      use m_precision
!
      use t_ucd_data
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine output_section_mesh(psf_header, itype_psf_file,        &
     &         psf_mesh, psf_out, psf_out_m)
!
      use t_psf_patch_data
      use set_ucd_data_to_type
      use parallel_ucd_IO_select
      use ucd_IO_select
      use merge_output_4_psf
!
      character(len = kchara), intent(in) :: psf_header
      integer(kind= kint), intent(in) :: itype_psf_file
      type(psf_local_data), intent(in) :: psf_mesh
      type(ucd_data), intent(inout) ::        psf_out
      type(merged_ucd_data), intent(inout) :: psf_out_m
!
!
      psf_out%file_prefix = psf_header
      psf_out%ifmt_file = itype_psf_file
!
      if((psf_out%ifmt_file/iflag_single) .eq. 0) then
        call merge_ucd_psf_mesh(psf_mesh, psf_out)
        if(my_rank .eq. 0) call sel_write_grd_file(iminus, psf_out)
      else
        call link_nnod_stacks_type_2_output(nprocs,                     &
     &    psf_mesh%node, psf_mesh%patch, psf_out_m)
!
        call link_ele_data_type_2_output(psf_mesh%patch, psf_out)
        call link_field_data_type_2_output(psf_mesh%node%numnod,        &
     &    psf_mesh%field, psf_out)
!
        call link_node_data_type_2_output(psf_mesh%node, psf_out)
        call sel_write_parallel_ucd_mesh(psf_out, psf_out_m)
      end if
!
      end subroutine output_section_mesh
!
!  ---------------------------------------------------------------------
!
      subroutine output_section_data                                    &
     &         (istep_psf, psf_mesh, psf_out, psf_out_m)
!
      use set_ucd_data_to_type
      use parallel_ucd_IO_select
      use ucd_IO_select
      use merge_output_4_psf
!
      integer(kind= kint), intent(in) ::  istep_psf
      type(psf_local_data), intent(in) :: psf_mesh
      type(ucd_data), intent(inout) :: psf_out
      type(merged_ucd_data), intent(inout) :: psf_out_m
!
!
      if((psf_out%ifmt_file/iflag_single) .eq. 0) then
        call merge_ucd_psf_data(psf_mesh, psf_out)
!
        if(my_rank .eq. 0) then
          call sel_write_udt_file(iminus, istep_psf, psf_out)
        end if
      else
         call sel_write_parallel_ucd_file                               &
     &      (istep_psf, psf_out, psf_out_m)
      end if
!
      end subroutine output_section_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine output_isosurface                                      &
     &         (iso_header, itype_iso_file, istep_iso, iso_mesh)
!
      use t_psf_patch_data
      use set_ucd_data_to_type
      use parallel_ucd_IO_select
      use ucd_IO_select
      use merge_output_4_psf
!
      character(len = kchara), intent(in) :: iso_header
      integer(kind= kint), intent(in) :: istep_iso, itype_iso_file
      type(psf_local_data), intent(in) :: iso_mesh
!
!>      Structure for isosurface output (used by master process)
      type(ucd_data) :: iso_out
      type(merged_ucd_data) :: iso_out_m
!
!
      iso_out%file_prefix = iso_header
      iso_out%ifmt_file = itype_iso_file
!
      if((iso_out%ifmt_file/iflag_single) .eq. 0) then
        call merge_ucd_psf_mesh(iso_mesh, iso_out)
        call merge_ucd_psf_data(iso_mesh, iso_out)
!
        if(my_rank .eq. 0) then
          call sel_write_ucd_file(iminus, istep_iso, iso_out)
          call deallocate_ucd_mesh(iso_out)
        end if
      else
        call link_nnod_stacks_type_2_output(nprocs,                     &
     &      iso_mesh%node, iso_mesh%patch, iso_out_m)
!
        call link_node_data_type_2_output(iso_mesh%node, iso_out)
        call link_ele_data_type_2_output(iso_mesh%patch, iso_out)
        call link_field_data_type_2_output(iso_mesh%node%numnod,        &
     &    iso_mesh%field, iso_out)
!
        call sel_write_parallel_ucd_file                                &
     &      (istep_iso, iso_out, iso_out_m)
        call disconnect_merged_ucd_mesh(iso_out, iso_out_m)
      end if
!
!
      end subroutine output_isosurface
!
!  ---------------------------------------------------------------------
!
      end module output_4_psf
