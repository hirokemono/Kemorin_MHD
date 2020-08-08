!>@file  time_step_file_IO.f90
!!       module time_step_file_IO
!!
!!@author H. Matsui and H.Okuda
!!@date   Programmed in July 2000 (ver 1.1)
!!@n      Modified in Aug., 2007
!
!> @brief Labels of fields
!!
!!@verbatim
!!      subroutine write_SGS_MHD_monitor_labels                         &
!!     &         (id_ave, id_msq, iphys, iphys_LES, msq_list)
!!      subroutine write_MHD_monitor_labels                             &
!!     &          (id_ave, id_msq, iphys, iphys_LES, msq_list)
!!        type(phys_address), intent(in) :: iphys
!!        type(SGS_model_addresses), intent(in) :: iphys_LES
!!        type(mean_square_list), intent(in) :: msq_list
!!@endverbatim
!
      module time_step_file_IO
!
      use m_precision
!
      implicit none
!
      private :: write_SGS_MHD_monitor_label, write_MHD_monitor_label
      private :: sym_tensor_label_4_step
      private :: vector_label_4_step, scalar_label_4_step
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine write_SGS_MHD_monitor_labels                           &
     &          (id_ave, id_msq, iphys, iphys_LES, msq_list)
!
      use t_phys_data
      use t_phys_address
      use t_SGS_model_addresses
      use t_mean_square_filed_list
!
      use m_time_labels
      use m_phys_constants
      use m_filtered_field_labels
      use m_volume_average_labels
      use write_field_labels
!
      integer (kind=kint), intent(in) :: id_ave, id_msq
      type(phys_address), intent(in) :: iphys
      type(SGS_model_addresses), intent(in) :: iphys_LES
      type(mean_square_list), intent(in) :: msq_list
!
      integer (kind=kint) :: i
!
!
      call write_one_label(id_msq, fhd_t_step)
      call write_one_label(id_ave, fhd_t_step)
      call write_one_label(id_msq, fhd_time)
      call write_one_label(id_ave, fhd_time)
!
      do i = 1, msq_list%nfield
        call write_SGS_MHD_monitor_label(id_ave, id_msq,                &
     &      iphys%base, iphys_LES%filter_fld, iphys%forces,             &
     &      iphys_LES%SGS_term, msq_list%ifld_msq(i),                   &
     &      msq_list%ncomp_msq(i), msq_list%field_name(i))
      end do
!
      call write_one_label(id_msq, e_hd_volume)
!
      write(id_msq,*)
      write(id_ave,*)
!
      end subroutine write_SGS_MHD_monitor_labels
!
! ----------------------------------------------------------------------
!
      subroutine write_MHD_monitor_labels                               &
     &          (id_ave, id_msq, iphys, iphys_LES, msq_list)
!
      use t_phys_data
      use t_base_field_labels
      use t_base_force_labels
      use t_SGS_term_labels
      use t_mean_square_filed_list
      use t_SGS_model_addresses
!
      use m_time_labels
      use m_phys_constants
      use m_filtered_field_labels
      use m_volume_average_labels
      use write_field_labels
!
      integer (kind=kint), intent(in) :: id_ave, id_msq
      type(phys_address), intent(in) :: iphys
      type(SGS_model_addresses), intent(in) :: iphys_LES
      type(mean_square_list), intent(in) :: msq_list
!
      integer (kind=kint) :: i
!
!
      call write_one_label(id_msq, fhd_t_step)
      call write_one_label(id_ave, fhd_t_step)
      call write_one_label(id_msq, fhd_time)
      call write_one_label(id_ave, fhd_time)
!
      do i = 1, msq_list%nfield
        call write_MHD_monitor_label(id_ave, id_msq,                    &
     &      iphys%base, iphys_LES%filter_fld, iphys%forces,             &
     &      msq_list%ifld_msq(i), msq_list%ncomp_msq(i),                &
     &      msq_list%field_name(i))
      end do
!
      call write_one_label(id_msq, e_hd_volume)
!
      write(id_msq,*)
      write(id_ave,*)
!
      end subroutine write_MHD_monitor_labels
!
! ----------------------------------------------------------------------
!
      subroutine write_SGS_MHD_monitor_label(id_ave, id_msq,            &
     &          iphys_base, iphys_fil, iphys_frc, iphys_SGS,            &
     &          ifld_msq, ncomp_msq, msq_name)
!
      use t_base_field_labels
      use t_base_force_labels
      use t_SGS_term_labels
!
      use m_phys_constants
      use m_filtered_field_labels
      use m_volume_average_labels
      use write_field_labels
!
      integer (kind=kint), intent(in) :: id_ave, id_msq
      type(base_field_address), intent(in) :: iphys_base
      type(base_field_address), intent(in) :: iphys_fil
      type(base_force_address), intent(in) :: iphys_frc
      type(SGS_term_address), intent(in) :: iphys_SGS
      integer(kind=kint), intent(in) :: ifld_msq, ncomp_msq
      character(len=kchara), intent(in) :: msq_name
!
      character(len=kchara) :: vector_label(3)
!
!
      if(ifld_msq .eq. iphys_SGS%i_SGS_induct_t) then
        call write_one_label(id_msq, msq_name)
        call set_asym_tensor_label(msq_name, vector_label)
        call write_vector_label(id_ave, vector_label)
      else
        call write_MHD_monitor_label                                    &
     &     (id_ave, id_msq, iphys_base, iphys_fil, iphys_frc,           &
     &      ifld_msq, ncomp_msq, msq_name)
      end if
!
      end subroutine write_SGS_MHD_monitor_label
!
! ----------------------------------------------------------------------
!
      subroutine write_MHD_monitor_label(id_ave, id_msq,                &
     &          iphys_base, iphys_fil, iphys_frc,                       &
     &          ifld_msq, ncomp_msq, msq_name)
!
      use t_base_field_labels
      use t_base_force_labels
      use t_SGS_term_labels
!
      use m_phys_constants
      use m_base_field_labels
      use m_filtered_field_labels
      use m_volume_average_labels
      use write_field_labels
!
      integer (kind=kint), intent(in) :: id_ave, id_msq
      type(base_field_address), intent(in) :: iphys_base
      type(base_field_address), intent(in) :: iphys_fil
      type(base_force_address), intent(in) :: iphys_frc
      integer(kind=kint), intent(in) :: ifld_msq, ncomp_msq
      character(len=kchara), intent(in) :: msq_name
!
      character(len=kchara) :: tmpchara
      character(len=kchara) :: vector_label(3)
!
!
      if(ifld_msq .eq. iphys_base%i_velo) then
        if(msq_name .eq. velocity%name) then
          call set_vector_label(msq_name, vector_label)
          call write_vector_label(id_ave, vector_label)
          call write_one_label(id_msq, e_hd_k_ene)
!
          call scalar_label_4_step(id_ave, id_msq, e_hd_div_v)
          call write_vector_label(id_ave, e_hd_lvec)
        else if(msq_name .eq. e_hd_div_v) then
          call scalar_label_4_step(id_ave, id_msq, e_hd_div_v)
        end if
!
      else if(ifld_msq .eq. iphys_base%i_vort) then
        write(tmpchara,'(a)') trim(vorticity%name)
        call vector_label_4_step(id_ave, id_msq, tmpchara)
        call write_one_label(id_msq, e_hd_rms_w)
!
      else if(ifld_msq .eq. iphys_base%i_vecp) then
        if(msq_name .eq. vector_potential%name) then
          call vector_label_4_step(id_ave, id_msq, msq_name)
          call scalar_label_4_step(id_ave, id_msq, e_hd_div_a)
        else if(msq_name .eq. e_hd_div_a) then
          call scalar_label_4_step(id_ave, id_msq, e_hd_div_a)
        end if
!
      else if(ifld_msq .eq. iphys_base%i_magne) then
        if(msq_name .eq. magnetic_field%name) then
          call write_three_labels                                       &
     &       (id_msq, e_hd_m_ene, e_hd_m_ene_cd, e_hd_div_b)
!
          call set_vector_label(msq_name, vector_label)
          call write_vector_label(id_ave, vector_label)
          call write_vector_label(id_ave, e_hd_bvec_cd)
          call write_one_label(id_ave, e_hd_div_b)
        else if(msq_name .eq. e_hd_div_b) then
          call scalar_label_4_step(id_ave, id_msq, e_hd_div_b)
        end if
!
      else if(ifld_msq .eq. iphys_base%i_current) then
        write(tmpchara,'(a)') trim(current_density%name)
        call vector_label_4_step(id_ave, id_msq, tmpchara)
        call vector_label_4_step(id_ave, id_msq, e_hd_sq_j_cd)
        call write_one_label(id_msq, e_hd_rms_j)
        call write_one_label(id_msq, e_hd_rms_j_cd)
!
      else if(ifld_msq .eq. iphys_fil%i_velo) then
        if(msq_name .eq. filter_velocity%name) then
          call write_one_label(id_msq, e_hd_fil_k_ene)
          call write_one_label(id_msq, e_hd_fil_div_v)
!
          call set_vector_label(msq_name, vector_label)
          call write_vector_label(id_ave, vector_label)
          call write_one_label(id_ave, e_hd_fil_div_v)
          call write_vector_label(id_ave, e_hd_fil_lvec)
        else if(msq_name .eq. e_hd_fil_div_v) then
          call scalar_label_4_step(id_ave, id_msq, e_hd_fil_div_v)
        end if
!
      else if(ifld_msq .eq. iphys_fil%i_vecp) then
        if(msq_name .eq. filter_vector_potential%name) then
          call vector_label_4_step(id_ave, id_msq, msq_name)
          call scalar_label_4_step(id_ave, id_msq, e_hd_fil_div_a)
        else if(msq_name .eq. e_hd_fil_div_a) then
          call scalar_label_4_step(id_ave, id_msq, e_hd_fil_div_a)
        end if
!
      else if(ifld_msq .eq. iphys_fil%i_magne) then
        if(msq_name .eq. filter_magne%name) then
          call write_three_labels(id_msq,                               &
     &        e_hd_fil_m_ene, e_hd_fil_m_ene_cd, e_hd_fil_div_b)
!
          call set_vector_label(msq_name, vector_label)
          call write_vector_label(id_ave, vector_label)
          call write_vector_label(id_ave, e_hd_fil_bvec_cd)
          call write_one_label(id_ave, e_hd_fil_div_b)
        else if(msq_name .eq. e_hd_fil_div_b) then
          call scalar_label_4_step(id_ave, id_msq, msq_name)
        end if
!
      else if(ifld_msq .eq. iphys_frc%i_induct_t) then
        call write_one_label(id_msq, msq_name)
        call set_asym_tensor_label(msq_name, vector_label)
        call write_vector_label(id_ave, vector_label)
!
      else if(ncomp_msq .eq. n_scalar) then
        call scalar_label_4_step(id_ave, id_msq, msq_name)
      else if(ncomp_msq .eq. n_vector) then
        call vector_label_4_step(id_ave, id_msq, msq_name)
      else if(ncomp_msq .eq. n_sym_tensor) then
        call sym_tensor_label_4_step(id_ave, id_msq, msq_name)
      end if
!
      end subroutine write_MHD_monitor_label
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine vector_label_4_step(id_ave, id_msq, vector_name)
!
      use m_volume_average_labels
      use write_field_labels
!
      integer (kind=kint), intent(in) :: id_ave, id_msq
      character(len=kchara), intent(in) :: vector_name
      character(len=kchara) :: vector_label(3)
!
!
      call set_vector_label(vector_name, vector_label)
      call write_one_label(id_msq, vector_name)
      call write_vector_label(id_ave, vector_label)
!
      end subroutine vector_label_4_step
!
! ----------------------------------------------------------------------
!
      subroutine scalar_label_4_step(id_ave, id_msq, scalar_name)
!
      use m_volume_average_labels
      use write_field_labels
!
      integer (kind=kint), intent(in) :: id_ave, id_msq
      character(len=kchara), intent(in) :: scalar_name
!
!
      call write_one_label(id_msq, scalar_name)
      call write_one_label(id_ave, scalar_name)
!
      end subroutine scalar_label_4_step
!
! ----------------------------------------------------------------------
!
      subroutine sym_tensor_label_4_step(id_ave, id_msq, tensor_name)
!
      use m_volume_average_labels
      use write_field_labels
!
      integer (kind=kint), intent(in) :: id_ave, id_msq
      character(len=kchara), intent(in) :: tensor_name
      character(len=kchara) :: tensor_label(6)
!
!
      call set_sym_tensor_label(tensor_name, tensor_label)
      call write_one_label(id_msq, tensor_name)
      call write_sym_tensor_label(id_ave, tensor_label)
!
      end subroutine sym_tensor_label_4_step
!
! ----------------------------------------------------------------------
!
      end module time_step_file_IO
