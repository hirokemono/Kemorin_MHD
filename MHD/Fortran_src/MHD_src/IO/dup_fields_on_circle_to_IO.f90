!>@file   dup_fields_on_circle_to_IO.f90
!!@brief  module dup_fields_on_circle_to_IO
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in June., 2011
!
!>@brief  Dynamo benchmark results
!!
!!@verbatim
!!      subroutine write_fields_on_circle_file                          &
!!     &         (my_rank, file_prefix, gzip_flag,                      &
!!     &          sph_params, time_d, cdat)
!!        integer, intent(in) :: my_rank
!!        character(len=kchara), intent(in) :: file_prefix
!!        logical, intent(in) :: gzip_flag
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(time_data), intent(in) :: time_d
!!        type(circle_fld_maker), intent(in) :: cdat
!!@endverbatim
!!
!!@param i_step   time step
!!@param time     time
!
      module dup_fields_on_circle_to_IO
!
      use m_precision
      use m_constants
!
      use t_spheric_parameter
      use t_phys_address
      use t_base_field_labels
      use t_read_sph_spectra
      use t_time_data
      use t_phys_data
      use t_field_on_circle
!
      implicit none
!
      integer(kind = kint), parameter, private :: id_circle = 36
!
      type(sph_spectr_head_labels), parameter                           &
     &            :: circle_field_labels = sph_spectr_head_labels(      &
     &                           hdr_nri = 'Number_of_grids',           &
     &                           hdr_ltr = 'truncation',                &
     &                           hdr_ICB_id = 'ICB_id',                 &
     &                           hdr_CMB_id = 'CMB_id',                 &
     &                           hdr_kr_in =  'Not_used',               &
     &                           hdr_r_in =   'Cylindrical_radius',     &
     &                           hdr_kr_out = 'Not_used',               &
     &                           hdr_r_out =  'Height_z',               &
     &                           hdr_num_field = 'Number_of_field',     &
     &                           hdr_num_comp = 'Number_of_components')
!
      private :: dup_field_on_circ_header_to_IO
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine write_fields_on_circle_file                            &
     &         (my_rank, file_prefix, gzip_flag,                        &
     &          sph_params, time_d, cdat)
!
      use t_buffer_4_gzip
      use set_parallel_file_name
      use sph_monitor_data_text
      use gz_open_sph_layer_mntr_file
      use gz_layer_mean_monitor_IO
!
      integer, intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_prefix
      logical, intent(in) :: gzip_flag
      type(sph_shell_parameters), intent(in) :: sph_params
      type(time_data), intent(in) :: time_d
      type(circle_fld_maker), intent(in) :: cdat
!
      logical :: flag_gzip_lc
      character(len = kchara) :: file_name
      type(buffer_4_gzip) :: zbuf_d
      type(read_sph_spectr_data) :: sph_OUT_d
!
      real(kind = kreal), allocatable :: spectr_IO(:,:)
!
!
      if(file_prefix .eq. 'NO_FILE') return
      if(my_rank .ne. 0) return
!
      call dup_field_on_circ_header_to_IO(sph_params,                   &
     &    cdat%circle, cdat%d_circle, sph_OUT_d)
!
      allocate(spectr_IO(cdat%d_circle%ntot_phys_viz,                   &
     &                   cdat%circle%mphi_circle))
!
      flag_gzip_lc = gzip_flag
      file_name = add_dat_extension(file_prefix)
      call sel_open_sph_fld_on_circle_file                              &
     &   (id_circle, file_name, circle_field_labels, cdat%circle,       &
     &    sph_OUT_d, zbuf_d, flag_gzip_lc)
      call swap_layer_mean_to_IO(cdat%circle%mphi_circle,               &
     &    cdat%d_circle%ntot_phys_viz, cdat%d_circle%d_fld,             &
     &    spectr_IO(1,1))
      call sel_gz_write_layer_mean_mtr                                  &
     &   (flag_gzip_lc, id_circle, time_d%i_time_step, time_d%time,     &
     &    cdat%circle%mphi_circle, cdat%mphi_list, cdat%phi_list,       &
     &    cdat%d_circle%ntot_phys_viz, spectr_IO(1,1), zbuf_d)
      close(id_circle)
!
      call dealloc_sph_espec_name(sph_OUT_d)
      deallocate(spectr_IO)
!
      end subroutine write_fields_on_circle_file
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dup_field_on_circ_header_to_IO                         &
     &         (sph_params, circle, d_circle, sph_OUT)
!
      use m_time_labels
      use sel_comp_labels_by_coord
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(fields_on_circle), intent(in) :: circle
      type(phys_data), intent(in) :: d_circle
!
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
!
      integer(kind = kint) :: ifld, ist
!
!
      if(allocated(sph_OUT%ene_sph_spec_name)) return
!
      sph_OUT%ltr_sph = sph_params%l_truncation
      sph_OUT%nri_sph = circle%mphi_circle
      sph_OUT%nri_dat = 1
      sph_OUT%kr_ICB =  sph_params%nlayer_ICB
      sph_OUT%kr_CMB =  sph_params%nlayer_CMB
      sph_OUT%kr_inner = izero
      sph_OUT%kr_outer = izero
      sph_OUT%r_inner =  circle%s_circle
      sph_OUT%r_outer =  circle%z_circle
!
      sph_OUT%nfield_sph_spec = d_circle%num_phys_viz
      sph_OUT%ntot_sph_spec =   d_circle%ntot_phys_viz
!
      sph_OUT%num_time_labels = 4
      call alloc_sph_espec_name(sph_OUT)
!
      sph_OUT%ncomp_sph_spec(1:sph_OUT%nfield_sph_spec)                 &
     &           = d_circle%num_component(1:sph_OUT%nfield_sph_spec)
!
      sph_OUT%ene_sph_spec_name(1) = fhd_t_step
      sph_OUT%ene_sph_spec_name(2) = fhd_time
      sph_OUT%ene_sph_spec_name(3) = 'Longitude_ID'
      sph_OUT%ene_sph_spec_name(4) = 'Longitude'
      do ifld = 1, d_circle%num_phys_viz
        ist = d_circle%istack_component(ifld-1)                         &
     &       + sph_OUT%num_time_labels
        if(d_circle%num_component(ifld) .eq. n_sym_tensor) then
          call sel_coord_tensor_comp_labels(circle%iflag_circle_coord,  &
     &        d_circle%phys_name(ifld), sph_OUT%ene_sph_spec_name(ist))
        else if(d_circle%num_component(ifld) .eq. n_vector) then
          call sel_coord_vector_comp_labels(circle%iflag_circle_coord,  &
     &        d_circle%phys_name(ifld), sph_OUT%ene_sph_spec_name(ist))
        else
          write(sph_OUT%ene_sph_spec_name(ist),'(a)')                   &
     &                   trim(d_circle%phys_name(ifld))
        end if
      end do
!
      end subroutine dup_field_on_circ_header_to_IO
!
! ----------------------------------------------------------------------
!
      subroutine sel_open_sph_fld_on_circle_file(id_file, base_name,    &
     &          monitor_labels, circle, sph_OUT, zbuf, flag_gzip_lc)
!
      use select_gz_stream_file_IO
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara), intent(in) :: base_name
      type(sph_spectr_head_labels), intent(in) :: monitor_labels
      type(fields_on_circle), intent(in) :: circle
!
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
      type(buffer_4_gzip), intent(inout) :: zbuf
      logical, intent(inout) :: flag_gzip_lc
!
      character(len = kchara) :: fname
      logical :: flag_miss
!
!
      fname = base_name
      call check_gzip_or_ascii_file(base_name, fname,                   &
     &                              flag_gzip_lc, flag_miss)
!
      if(flag_miss) then
        write(*,*) 'Make file to write: ', trim(fname)
        open(id_file, file=fname, status='replace',                     &
     &       FORM='UNFORMATTED', ACCESS='STREAM')
        call write_sph_field_on_circle_head(flag_gzip_lc, id_file,      &
     &      monitor_labels, sph_OUT, zbuf, circle)
      else
        open(id_file, file=fname, status='old', position='append',      &
     &       FORM='UNFORMATTED', ACCESS='STREAM')
      end if
!
      end subroutine sel_open_sph_fld_on_circle_file
!
!  --------------------------------------------------------------------
!
      subroutine write_sph_field_on_circle_head(flag_gzip, id_file,     &
     &          monitor_labels, sph_OUT, zbuf, circle)
!
      use select_gz_stream_file_IO
      use sph_power_spectr_data_text
!
      logical, intent(in) :: flag_gzip
      integer(kind = kint), intent(in) :: id_file
      type(sph_spectr_head_labels), intent(in) :: monitor_labels
      type(read_sph_spectr_data), intent(in) :: sph_OUT
      type(buffer_4_gzip), intent(inout) :: zbuf
      type(fields_on_circle), intent(in) :: circle
!
      integer(kind = kint) :: len_each(6)
      integer(kind = kint) :: len_tot, len_line
!
!
      call len_sph_field_on_circle_header                               &
     &   (monitor_labels, sph_OUT, len_line, len_each, len_tot)
      call sel_gz_write_text_stream(flag_gzip, id_file,                 &
     &    sph_field_on_circle_text(len_tot, len_each, len_line,         &
     &                             monitor_labels, sph_OUT, circle),    &
     &    zbuf)
!
      end subroutine write_sph_field_on_circle_head
!
!   --------------------------------------------------------------------
!
      subroutine len_sph_field_on_circle_header                         &
     &         (lbl_OUT, sph_OUT, len_line, len_each, len_tot)
!
      use sph_monitor_header_text
!
      type(sph_spectr_head_labels), intent(in) :: lbl_OUT
      type(read_sph_spectr_data), intent(in) :: sph_OUT
      integer(kind = kint), intent(inout)  :: len_each(6)
      integer(kind = kint), intent(inout)  :: len_line, len_tot
!
!
      len_line = 27 + 16 + 1
      len_each(1) = len_moniter_i2_head_text(lbl_OUT%hdr_nri,           &
     &                                       lbl_OUT%hdr_ltr)
      len_each(2) = len_moniter_i2_head_text(lbl_OUT%hdr_ICB_id,        &
     &                                       lbl_OUT%hdr_CMB_id)
      len_each(3) = 0
      len_each(4) = 0
!
      len_each(5) = len_monitor_data_ncomps_text(lbl_OUT%hdr_num_field, &
     &                                         lbl_OUT%hdr_num_comp,    &
     &                                         sph_OUT%nfield_sph_spec)
      len_each(6) = len_data_names_text(sph_OUT%num_labels,             &
     &                                  sph_OUT%ene_sph_spec_name)
      len_tot = 4 + len_line + sum(len_each(1:6))
!
      end subroutine len_sph_field_on_circle_header
!
!   --------------------------------------------------------------------
!
      function sph_field_on_circle_text(len_header, len_each, len_line, &
     &                                  lbl_OUT, sph_OUT, circle)
!
      use sph_monitor_header_text
!
      integer(kind = kint), intent(in) :: len_header, len_line
      integer(kind = kint), intent(in) :: len_each(6)
      type(sph_spectr_head_labels), intent(in) :: lbl_OUT
      type(read_sph_spectr_data), intent(in) :: sph_OUT
      type(fields_on_circle), intent(in) :: circle
!
      character(len = len_header) :: sph_field_on_circle_text
!
      integer(kind = kint) :: ist
      character(len = len_header) :: textbuf
!
!
      write(textbuf(1:2),'(2a1)') "#", char(10)
      ist = 2
      write(textbuf(ist+1:ist+len_line),'(a27,1pE16.6e3,a1)')           &
     &     "#  Radius:             r = ", circle%r_circle, char(10)
      ist = ist + len_line
      write(textbuf(ist+1:ist+len_line),'(a27,1pE16.6e3,a1)')           &
     &     "#  Cylindrical Radius: s = ", circle%s_circle, char(10)
      ist = ist + len_line
      write(textbuf(ist+1:ist+len_line),'(a27,1pE16.6e3,a1)')           &
     &     "#  Vertical position:  z = ", circle%z_circle, char(10)
      ist = ist + len_line
      write(textbuf(ist+1:ist+len_line),'(a27,1pE16.6e3,a1)')           &
     &     "#  Colatitude:         r = ", circle%colat_circle, char(10)
      ist = ist + len_line
      write(textbuf(ist+1:ist+len_line),'(2a1)') "#", char(10)
      ist = ist + 2
!
      textbuf(ist+1:ist+len_each(1))                                    &
     &     = monitor_int2_head_text(len_each(1),                        &
     &                              lbl_OUT%hdr_nri, lbl_OUT%hdr_ltr,   &
     &                              sph_OUT%nri_sph, sph_OUT%ltr_sph)
      ist = ist + len_each(1)
!
      textbuf(ist+1:ist+len_each(2))                                    &
     &     = monitor_int2_head_text(len_each(2),                        &
     &                          lbl_OUT%hdr_ICB_id, lbl_OUT%hdr_CMB_id, &
     &                          sph_OUT%kr_ICB, sph_OUT%kr_CMB)
      ist = ist + len_each(2)
!
      textbuf(ist+1:ist+len_each(5))                                    &
     &     = monitor_data_ncomps_text(len_each(5),                      &
     &                  lbl_OUT%hdr_num_field, lbl_OUT%hdr_num_comp,    &
     &                  sph_OUT%nfield_sph_spec, sph_OUT%ntot_sph_spec, &
     &                  sph_OUT%ncomp_sph_spec)
      ist = ist + len_each(5)
!
      textbuf(ist+1:ist+len_each(6))                                    &
     &     = monitor_data_names_text(len_each(6), sph_OUT%num_labels,   &
     &                               sph_OUT%ene_sph_spec_name)
      sph_field_on_circle_text = textbuf
!
      end function sph_field_on_circle_text
!
!   --------------------------------------------------------------------
!
      end module dup_fields_on_circle_to_IO
