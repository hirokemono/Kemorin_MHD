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
!!      subroutine output_monitor_file(my_rank)
!!      subroutine skip_time_step_data(my_rank)
!!@endverbatim
!
      module time_step_file_IO
!
      use m_precision
!
      implicit none
!
!>        File ID for average data
      integer(kind=kint), parameter :: time_step_data_code = 41
!>        File ID for mean square data
      integer(kind=kint), parameter :: rms_data_code =       43
! 
!>        File name for average data
      character(len=kchara), parameter                                  &
     &       :: volume_ave_file_name =     'time_step_data.dat'
!>        File name for mean square data
      character(len=kchara), parameter                                  &
     &       :: volume_rms_file_name =     'time_rms_data.dat'
!
!
      private :: time_step_data_code,  rms_data_code
      private :: volume_ave_file_name, volume_rms_file_name
      private :: open_monitor_file, sym_tensor_label_4_step
      private :: vector_label_4_step, scalar_label_4_step
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine output_monitor_file(my_rank)
!
      use m_t_step_parameter
      use m_bulk_values
!
      integer (kind=kint), intent(in) :: my_rank
!
!
      if ( my_rank .gt. 0 ) return
!
      call open_monitor_file(my_rank)
!
      write(time_step_data_code,'(i16,1p1000e20.11)')                   &
     &     i_step_MHD, time, bulk_global(1:num_bulk)
      write(rms_data_code,'(i16,1p100e20.11)')                          &
     &     i_step_MHD, time, rms_global(1:num_rms)
!
      close(time_step_data_code)
      close(rms_data_code)
!
      end subroutine output_monitor_file
!
! ----------------------------------------------------------------------
!
      subroutine skip_time_step_data(my_rank)
!
      use m_t_step_parameter
      use m_bulk_values
!
      integer (kind=kint), intent(in) :: my_rank
!
      integer (kind = kint) :: i, iflag, i_read_step
      real(kind = kreal) :: rtmp
!
!
      if(my_rank .gt. 0) return
      iflag = i_step_init - mod(istep_max_dt, i_step_check)
!
      do
        read(time_step_data_code,*,err=99,end=99)                       &
     &            i_read_step, rtmp, (rtmp,i=1,num_bulk)
        if (i_read_step .ge. i_step_init) exit
      end do
 99   continue
!
      do
        read(rms_data_code,*,err=98,end=98)                             &
     &            i_read_step, rtmp, (rtmp,i=1,num_rms)
        if (i_read_step .ge. iflag) exit
      end do
 98   continue
!
      end subroutine skip_time_step_data
!
!  ---------------------------------------------------------------------
!
      subroutine open_monitor_file (my_rank)
!
      use m_node_phys_data
      use m_phys_labels
      use m_volume_average_labels
      use write_field_labels
!
      integer (kind=kint), intent(in) :: my_rank
      character(len=kchara) :: vector_label(3)
      integer (kind=kint) :: i
!
!
!
      if ( my_rank .ne. 0 ) return
!
!   If data files exist, append data at the end of file
!
      open (time_step_data_code,file = volume_ave_file_name,          &
     &      status='old', position='append', err = 99)
      open (rms_data_code,file = volume_rms_file_name,                &
     &      status='old', position='append', err = 98)
      return
!
!   If data files does not exist, create new data file
!
   98 continue
      close(time_step_data_code)
   99 continue
!
      open (time_step_data_code,file = volume_ave_file_name,            &
     &      status='replace')
      open (rms_data_code,file = volume_rms_file_name,                  &
     &      status='replace')
!
!
      call write_one_label(rms_data_code,       fhd_t_step)
      call write_one_label(time_step_data_code, fhd_t_step)
      call write_one_label(rms_data_code,       fhd_time)
      call write_one_label(time_step_data_code, fhd_time)
!
      do i = 1, nod_fld1%num_phys
        if (nod_fld1%iflag_monitor(i) .eq. 1) then
          if ( nod_fld1%phys_name(i) .eq. fhd_velo ) then
            call write_one_label(rms_data_code, e_hd_k_ene)
            call write_one_label(rms_data_code, e_hd_div_v)
!
            call set_vector_label(fhd_velo, vector_label)
            call write_vector_label(time_step_data_code, vector_label)
            call write_one_label(time_step_data_code, e_hd_div_v)
            call write_vector_label(time_step_data_code, e_hd_lvec)
!
          else if ( nod_fld1%phys_name(i) .eq. fhd_magne) then
            call write_three_labels(rms_data_code,                      &
     &            e_hd_m_ene, e_hd_m_ene_cd, e_hd_div_b)
!
            call set_vector_label(fhd_magne, vector_label)
            call write_vector_label(time_step_data_code, vector_label)
            call write_vector_label(time_step_data_code, e_hd_bvec_cd)
            call write_one_label(time_step_data_code, e_hd_div_b)
!
          else if ( nod_fld1%phys_name(i) .eq. fhd_vecp) then
            call write_one_label(rms_data_code,       e_hd_div_a)
            call write_one_label(time_step_data_code, e_hd_div_a)
!
          else if ( nod_fld1%phys_name(i) .eq. fhd_vort) then
            call vector_label_4_step(nod_fld1%phys_name(i), fhd_vort)
            call write_one_label(rms_data_code, e_hd_rms_w)
!
          else if ( nod_fld1%phys_name(i) .eq. fhd_current) then
            call vector_label_4_step                                    &
     &         (nod_fld1%phys_name(i), fhd_current)
            call vector_label_4_step(e_hd_sq_j_cd, e_hd_sq_j_cd)
            call write_one_label(rms_data_code, e_hd_rms_j)
            call write_one_label(rms_data_code, e_hd_rms_j_cd)
!
          else if ( nod_fld1%phys_name(i) .eq. fhd_filter_v ) then
            call write_one_label(rms_data_code, e_hd_fil_k_ene)
            call write_one_label(rms_data_code, e_hd_fil_div_v)
!
            call set_vector_label(fhd_filter_v, vector_label)
            call write_vector_label(time_step_data_code, vector_label)
            call write_one_label(time_step_data_code, e_hd_fil_div_v)
            call write_vector_label(time_step_data_code, e_hd_fil_lvec)
!
          else if ( nod_fld1%phys_name(i) .eq. fhd_filter_b ) then
            call write_three_labels(rms_data_code,                      &
     &            e_hd_fil_m_ene, e_hd_fil_m_ene_cd, e_hd_fil_div_b)
!
            call set_vector_label(fhd_filter_b, vector_label)
            call write_vector_label(time_step_data_code, vector_label)
            call write_vector_label(time_step_data_code,                &
     &          e_hd_fil_bvec_cd)
            call write_one_label(time_step_data_code, e_hd_fil_div_b)
!
          else if ( nod_fld1%phys_name(i) .eq. fhd_filter_a ) then
            call write_one_label(rms_data_code,       e_hd_fil_div_a)
            call write_one_label(time_step_data_code, e_hd_fil_div_a)
!
          else if ( nod_fld1%phys_name(i) .eq. fhd_induct_t ) then
            call write_one_label(rms_data_code, fhd_induct_t)
            call set_asym_tensor_label(fhd_induct_t, vector_label)
            call write_vector_label(time_step_data_code, vector_label)
!
          else if ( nod_fld1%phys_name(i) .eq. fhd_SGS_induct_t ) then
            call write_one_label(rms_data_code, fhd_SGS_induct_t)
            call set_asym_tensor_label(fhd_SGS_induct_t, vector_label)
            call write_vector_label(time_step_data_code, vector_label)
!
!    Old field label... Shold be deleted...
          else if ( nod_fld1%phys_name(i) .eq. fhd_buoyancy_work ) then
            call write_one_label(rms_data_code, fhd_buoyancy_work)
            call write_one_label(time_step_data_code,                   &
     &            fhd_buoyancy_work)
          end if
!
!
          call scalar_label_4_step(nod_fld1%phys_name(i), fhd_temp)
          call scalar_label_4_step                                      &
     &       (nod_fld1%phys_name(i), fhd_part_temp)
          call scalar_label_4_step(nod_fld1%phys_name(i), fhd_light)
          call scalar_label_4_step                                      &
     &       (nod_fld1%phys_name(i), fhd_part_light)
          call scalar_label_4_step(nod_fld1%phys_name(i), fhd_press)
          call scalar_label_4_step                                      &
     &       (nod_fld1%phys_name(i), fhd_mag_potential)
!
          call scalar_label_4_step(nod_fld1%phys_name(i), fhd_entropy)
          call scalar_label_4_step                                      &
     &       (nod_fld1%phys_name(i), fhd_per_entropy)
          call scalar_label_4_step(nod_fld1%phys_name(i), fhd_density)
          call scalar_label_4_step                                      &
     &       (nod_fld1%phys_name(i), fhd_per_density)
!
          call scalar_label_4_step                                      &
     &       (nod_fld1%phys_name(i), fhd_heat_source)
          call scalar_label_4_step                                      &
     &       (nod_fld1%phys_name(i), fhd_light_source)
          call scalar_label_4_step                                      &
     &       (nod_fld1%phys_name(i), fhd_entropy_source)
!
          call vector_label_4_step(nod_fld1%phys_name(i), fhd_e_field)
          call vector_label_4_step(nod_fld1%phys_name(i), fhd_poynting)
!
          call scalar_label_4_step(nod_fld1%phys_name(i),               &
     &        fhd_buoyancy_flux)
          call scalar_label_4_step(nod_fld1%phys_name(i),               &
     &        fhd_comp_buo_flux)
          call scalar_label_4_step(nod_fld1%phys_name(i),               &
     &        fhd_filter_buo_flux)
          call scalar_label_4_step(nod_fld1%phys_name(i),               &
     &        fhd_Lorentz_work)
          call scalar_label_4_step(nod_fld1%phys_name(i),               &
     &        fhd_work_agst_Lorentz)
          call scalar_label_4_step(nod_fld1%phys_name(i),               &
     &        fhd_mag_tension_work)
          call scalar_label_4_step(nod_fld1%phys_name(i),               &
     &        fhd_mag_ene_gen)
!
          call vector_label_4_step(nod_fld1%phys_name(i), fhd_grad_v_1)
          call vector_label_4_step(nod_fld1%phys_name(i), fhd_grad_v_2)
          call vector_label_4_step(nod_fld1%phys_name(i), fhd_grad_v_3)
!
          call vector_label_4_step                                      &
     &       (nod_fld1%phys_name(i), fhd_press_grad)
!
          call vector_label_4_step                                      &
     &       (nod_fld1%phys_name(i), fhd_mag_tension)
!
          call vector_label_4_step(nod_fld1%phys_name(i), fhd_inertia)
          call vector_label_4_step(nod_fld1%phys_name(i),               &
     &        fhd_div_m_flux)
          call vector_label_4_step(nod_fld1%phys_name(i),               &
     &        fhd_div_maxwell_t)
          call vector_label_4_step(nod_fld1%phys_name(i),               &
     &        fhd_div_induct_t)
!
          call vector_label_4_step                                      &
     &       (nod_fld1%phys_name(i), fhd_vp_induct)
          call vector_label_4_step                                      &
     &       (nod_fld1%phys_name(i), fhd_mag_stretch)
!
          call vector_label_4_step                                      &
     &       (nod_fld1%phys_name(i), fhd_vecp_diffuse)
          call vector_label_4_step                                      &
     &       (nod_fld1%phys_name(i), fhd_mag_diffuse)
          call vector_label_4_step(nod_fld1%phys_name(i), fhd_viscous)
!
          call scalar_label_4_step(nod_fld1%phys_name(i),               &
     &        fhd_vis_ene_diffuse)
          call scalar_label_4_step(nod_fld1%phys_name(i),               &
     &        fhd_mag_ene_diffuse)
          call scalar_label_4_step(nod_fld1%phys_name(i),               &
     &        fhd_thermal_diffusion)
          call scalar_label_4_step(nod_fld1%phys_name(i),               &
     &        fhd_c_diffuse)
!
          call vector_label_4_step(nod_fld1%phys_name(i), fhd_Lorentz)
          call vector_label_4_step(nod_fld1%phys_name(i), fhd_Coriolis)
          call vector_label_4_step(nod_fld1%phys_name(i), fhd_buoyancy)
          call vector_label_4_step(nod_fld1%phys_name(i), fhd_comp_buo)
          call vector_label_4_step                                      &
     &       (nod_fld1%phys_name(i), fhd_filter_buo)
!
          call sym_tensor_label_4_step                                  &
     &       (nod_fld1%phys_name(i), fhd_mom_flux)
          call sym_tensor_label_4_step                                  &
     &       (nod_fld1%phys_name(i), fhd_maxwell_t)
          call vector_label_4_step(nod_fld1%phys_name(i), fhd_ph_flux)
          call vector_label_4_step(nod_fld1%phys_name(i), fhd_h_flux)
          call vector_label_4_step(nod_fld1%phys_name(i), fhd_c_flux)
!
!
          call scalar_label_4_step                                      &
     &       (nod_fld1%phys_name(i), fhd_heat_advect)
          call scalar_label_4_step                                      &
     &       (nod_fld1%phys_name(i), fhd_part_h_advect)
          call scalar_label_4_step                                      &
     &       (nod_fld1%phys_name(i), fhd_div_h_flux)
          call scalar_label_4_step                                      &
     &       (nod_fld1%phys_name(i), fhd_div_ph_flux)
          call scalar_label_4_step                                      &
     &       (nod_fld1%phys_name(i), fhd_temp_generation)
          call scalar_label_4_step                                      &
     &       (nod_fld1%phys_name(i), fhd_part_temp_gen)
!
!
          call scalar_label_4_step                                      &
     &       (nod_fld1%phys_name(i), fhd_filter_temp)
          call scalar_label_4_step                                      &
     &       (nod_fld1%phys_name(i), fhd_filter_temp)
          call scalar_label_4_step                                      &
     &       (nod_fld1%phys_name(i), fhd_filter_comp)
!
          call sym_tensor_label_4_step(nod_fld1%phys_name(i),           &
     &        fhd_SGS_m_flux)
          call sym_tensor_label_4_step(nod_fld1%phys_name(i),           &
     &        fhd_SGS_maxwell_t)
          call vector_label_4_step                                      &
     &       (nod_fld1%phys_name(i), fhd_SGS_h_flux)
          call vector_label_4_step                                      &
     &       (nod_fld1%phys_name(i), fhd_SGS_c_flux)
          call vector_label_4_step(nod_fld1%phys_name(i), fhd_c_flux)
!
          call vector_label_4_step                                      &
     &       (nod_fld1%phys_name(i), fhd_div_SGS_m_flux)
          call vector_label_4_step                                      &
     &       (nod_fld1%phys_name(i), fhd_SGS_Lorentz)
          call vector_label_4_step                                      &
     &       (nod_fld1%phys_name(i), fhd_SGS_induction)
          call vector_label_4_step                                      &
     &       (nod_fld1%phys_name(i), fhd_SGS_vp_induct)
          call vector_label_4_step                                      &
     &       (nod_fld1%phys_name(i), fhd_SGS_buoyancy)
          call vector_label_4_step                                      &
     &       (nod_fld1%phys_name(i), fhd_SGS_comp_buo)
!
          call scalar_label_4_step(nod_fld1%phys_name(i),               &
     &        fhd_div_SGS_h_flux)
          call scalar_label_4_step(nod_fld1%phys_name(i),               &
     &        fhd_SGS_temp_gen)
          call scalar_label_4_step(nod_fld1%phys_name(i),               &
     &        fhd_SGS_m_ene_gen)
          call scalar_label_4_step(nod_fld1%phys_name(i),               &
     &        fhd_SGS_Lorentz_work)
          call scalar_label_4_step(nod_fld1%phys_name(i),               &
     &        fhd_Reynolds_work)
          call scalar_label_4_step(nod_fld1%phys_name(i),               &
     &        fhd_SGS_buo_flux)
          call scalar_label_4_step(nod_fld1%phys_name(i),               &
     &        fhd_SGS_comp_buo_flux)
!
!
          call vector_label_4_step(nod_fld1%phys_name(i),               &
     &        fhd_SGS_div_m_flux_true)
          call vector_label_4_step(nod_fld1%phys_name(i),               &
     &        fhd_SGS_Lorentz_true)
          call vector_label_4_step(nod_fld1%phys_name(i),               &
     &        fhd_SGS_mag_induct_true)
!
          call scalar_label_4_step(nod_fld1%phys_name(i),               &
     &        fhd_SGS_div_h_flux_true)
          call scalar_label_4_step(nod_fld1%phys_name(i),               &
     &        fhd_SGS_Lorentz_wk_true)
          call scalar_label_4_step(nod_fld1%phys_name(i),               &
     &        fhd_Reynolds_work_true)
          call scalar_label_4_step(nod_fld1%phys_name(i),               &
     &        fhd_SGS_temp_gen_true)
          call scalar_label_4_step(nod_fld1%phys_name(i),               &
     &        fhd_SGS_m_ene_gen_true)
!
        else
!
          if ( nod_fld1%phys_name(i) .eq. fhd_velo) then
            call write_one_label(rms_data_code,       e_hd_div_v)
            call write_one_label(time_step_data_code, e_hd_div_v)
          else if ( nod_fld1%phys_name(i) .eq. fhd_magne) then
            call write_one_label(rms_data_code,       e_hd_div_b)
            call write_one_label(time_step_data_code, e_hd_div_b)
          else if ( nod_fld1%phys_name(i) .eq. fhd_vecp) then
            call write_one_label(rms_data_code,       e_hd_div_a)
            call write_one_label(time_step_data_code, e_hd_div_a)
!
          else if ( nod_fld1%phys_name(i) .eq. fhd_filter_v) then
            call write_one_label(rms_data_code,       e_hd_fil_div_v)
            call write_one_label(time_step_data_code, e_hd_fil_div_v)
          else if ( nod_fld1%phys_name(i) .eq. fhd_filter_a) then
            call write_one_label(rms_data_code,       e_hd_fil_div_a)
            call write_one_label(time_step_data_code, e_hd_fil_div_a)
          else if ( nod_fld1%phys_name(i) .eq. fhd_filter_b) then
            call write_one_label(rms_data_code,       e_hd_fil_div_b)
            call write_one_label(time_step_data_code, e_hd_fil_div_b)
          end if
!
          call scalar_label_4_step(nod_fld1%phys_name(i),               &
     &        fhd_mag_potential)
        end if
      end do
!
      call write_one_label(rms_data_code, e_hd_volume)
!
      write(rms_data_code,*)      
      write(time_step_data_code,*)
!
      end subroutine open_monitor_file
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine vector_label_4_step(phys_name, vector_name)
!
      use m_volume_average_labels
      use write_field_labels
!
      character(len=kchara), intent(in) :: phys_name, vector_name
      character(len=kchara) :: vector_label(3)
!
!
      if (phys_name .ne. vector_name)  return
!
      call set_vector_label(vector_name, vector_label)
      call write_one_label(rms_data_code, vector_name)
      call write_vector_label(time_step_data_code, vector_label)
!
      end subroutine vector_label_4_step
!
! ----------------------------------------------------------------------
!
      subroutine scalar_label_4_step(phys_name, scalar_name)
!
      use m_volume_average_labels
      use write_field_labels
!
      character(len=kchara), intent(in) :: phys_name, scalar_name
!
!
      if (phys_name .ne. scalar_name)  return
!
      call write_one_label(rms_data_code, scalar_name)
      call write_one_label(time_step_data_code, scalar_name)
!
      end subroutine scalar_label_4_step
!
! ----------------------------------------------------------------------
!
      subroutine sym_tensor_label_4_step(phys_name, tensor_name)
!
      use m_volume_average_labels
      use write_field_labels
!
      character(len=kchara), intent(in) :: phys_name, tensor_name
      character(len=kchara) :: tensor_label(6)
!
!
      if (phys_name .ne. tensor_name)  return
!
      call set_sym_tensor_label(tensor_name, tensor_label)
      call write_one_label(rms_data_code, tensor_name)
      call write_sym_tensor_label(time_step_data_code, tensor_label)
!
      end subroutine sym_tensor_label_4_step
!
! ----------------------------------------------------------------------
!
      end module time_step_file_IO
