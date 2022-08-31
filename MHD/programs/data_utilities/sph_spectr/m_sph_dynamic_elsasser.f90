!>@file   m_sph_dynamic_elsasser.f90
!!        module m_sph_dynamic_elsasser
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Obtain lengh scale from spherical harmonics spectrum data
!!
!!@verbatim
!!      subroutine sph_dynamic_elsasser_by_spectr                       &
!!     &         (input_header, sph_IN)
!!        type(read_sph_spectr_data), intent(inout) :: sph_IN
!!@endverbatim
!
      module m_sph_dynamic_elsasser
!
      use m_precision
      use m_constants
      use m_phys_constants
      use t_read_sph_spectra
      use t_buffer_4_gzip
!
      implicit none
!
      type sph_dyn_elsasser_data
        character(len = kchara)                                         &
     &               :: vol_l_spectr_file_prefix = 'ssph_pwr_volume_l'
        character(len = kchara)                                         &
     &               :: vol_m_spectr_file_prefix = 'ssph_pwr_volume_m'
        character(len = kchara) :: elsasser_file_prefix = 'Elsasser'
!
        logical :: flag_old_spectr_data = .FALSE.
!
        real(kind = kreal) :: start_time
        real(kind = kreal) :: end_time
!
        real(kind = kreal) :: ME_scale =      1.0d0
        real(kind = kreal) :: coef_elsasser = 1.0d0
        real(kind = kreal) :: mag_Re_ratio =  1.0d0
!
        integer(kind = kint) :: irms_KE =  0
        integer(kind = kint) :: irms_ME =  0
        integer(kind = kint) :: irms_T =   0
        integer(kind = kint) :: irms_C =   0
      end type sph_dyn_elsasser_data
!
      integer(kind = kint), parameter :: id_file_rms_l =    35
      integer(kind = kint), parameter :: id_file_rms_m =    37
      integer(kind = kint), parameter :: id_file_lscale =   44
!
      integer(kind = kint), parameter :: iflag_on = 1
!
      type(read_sph_spectr_data), save :: sph_IN_l, sph_IN_m
      type(read_sph_spectr_data), save :: sph_OUT1
!
      integer(kind = kint), private :: ist_KEne =  0
      integer(kind = kint), private :: ist_MEne =  0
      integer(kind = kint), private :: ist_Temp =  0
      integer(kind = kint), private :: ist_Comp =  0
!
      integer(kind = kint), private :: ist_Re =  0
      integer(kind = kint), private :: ist_Rm =  0
      integer(kind = kint), private :: ist_Elsasser = 0
      integer(kind = kint), private :: ist_dyn_Els =  0
!
      integer(kind = kint), private :: ist_ulength_l =  0
      integer(kind = kint), private :: ist_Blength_l =  0
      integer(kind = kint), private :: ist_Tlength_l =  0
      integer(kind = kint), private :: ist_Clength_l =  0
!
      integer(kind = kint), private :: ist_ulength_m =  0
      integer(kind = kint), private :: ist_Blength_m =  0
      integer(kind = kint), private :: ist_Tlength_m =  0
      integer(kind = kint), private :: ist_Clength_m =  0
!
      real(kind = kreal), parameter :: pi = four * atan(one)
      real(kind = kreal), parameter :: Lscale = 1.0
!
      logical, parameter, private :: vol_ave_on = .TRUE.
!
      private :: sph_OUT1, sph_IN_l, sph_IN_m
      private :: id_file_rms_l, id_file_rms_m, id_file_lscale
      private :: sum_ene_spectr, uli_sph_length_scale
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine sph_dynamic_elsasser_by_spectr(els_dat)
!
      use simple_sph_spectr_head_IO
      use sph_mean_square_IO
      use set_parallel_file_name
!
      type(sph_dyn_elsasser_data), intent(inout) :: els_dat
!
      logical :: flag_gzip_l, flag_gzip_m
      type(buffer_4_gzip) :: zbuf_l, zbuf_m
      character(len = kchara) :: file_name, fname_tmp
      integer(kind = kint) :: i, icou, jcou, ierr, ist_true, kcou, ifld
      real(kind = kreal) :: lscale_b
!
!
      file_name = add_dat_extension(els_dat%vol_l_spectr_file_prefix)
      call sel_open_read_sph_monitor_file(id_file_rms_l, file_name,     &
     &    flag_gzip_l, zbuf_l)
      call select_input_sph_series_head(id_file_rms_l, flag_gzip_l,     &
     &    els_dat%flag_old_spectr_data, spectr_on, vol_ave_on,          &
     &    sph_IN_l, zbuf_l)
      call check_sph_spectr_name(sph_IN_l)
!
      file_name = add_dat_extension(els_dat%vol_m_spectr_file_prefix)
      call sel_open_read_sph_monitor_file(id_file_rms_m, file_name,     &
     &    flag_gzip_m, zbuf_m)
      call select_input_sph_series_head(id_file_rms_m, flag_gzip_m,     &
     &    els_dat%flag_old_spectr_data, spectr_on, vol_ave_on,          &
     &    sph_IN_m, zbuf_m)
      call check_sph_spectr_name(sph_IN_m)
!
      do i = 1, sph_IN_l%num_labels
        if(sph_IN_l%ene_sph_spec_name(i) .eq. 'K_ene')                  &
     &                                     els_dat%irms_KE = i
        if(sph_IN_l%ene_sph_spec_name(i) .eq. 'M_ene')                  &
     &                                     els_dat%irms_ME = i
        if(sph_IN_l%ene_sph_spec_name(i) .eq. 'temperature')            &
     &                                     els_dat%irms_T = i
        if(sph_IN_l%ene_sph_spec_name(i) .eq. 'composition')            &
     &                                     els_dat%irms_C = i
      end do
!
      call copy_read_ene_head_params(sph_IN_l, sph_OUT1)
      jcou = 0
      ifld = 0
      if(els_dat%irms_KE .ge. 3) then
        els_dat%irms_KE = els_dat%irms_KE - sph_IN_l%num_time_labels
        call add_new_field_address(ifld, jcou, n_vector, ist_KEne)
        call add_new_field_address(ifld, jcou, n_scalar, ist_Re)
        call add_new_field_address(ifld, jcou, n_vector, ist_ulength_l)
        call add_new_field_address(ifld, jcou, n_vector, ist_ulength_m)
      end if
      if(els_dat%irms_ME .ge. 3) then
        els_dat%irms_ME = els_dat%irms_ME - sph_IN_l%num_time_labels
        call add_new_field_address(ifld, jcou, n_vector, ist_MEne)
        call add_new_field_address(ifld, jcou, n_scalar, ist_Elsasser)
        call add_new_field_address(ifld, jcou, n_vector, ist_Blength_l)
        call add_new_field_address(ifld, jcou, n_vector, ist_Blength_m)
      end if
      if(els_dat%irms_KE .ge. 3 .and. els_dat%irms_ME .ge. 3) then
        call add_new_field_address(ifld, jcou, n_scalar, ist_Rm)
        call add_new_field_address(ifld, jcou, n_scalar, ist_dyn_Els)
      end if
      if(els_dat%irms_T .ge. 1) then
        els_dat%irms_T =  els_dat%irms_T - sph_IN_l%num_time_labels
        call add_new_field_address(ifld, jcou, n_scalar, ist_Temp)
        call add_new_field_address(ifld, jcou, n_scalar, ist_Tlength_l)
        call add_new_field_address(ifld, jcou, n_scalar, ist_Tlength_m)
      end if
      if(els_dat%irms_C .ge. 1) then
        els_dat%irms_C =  els_dat%irms_C - sph_IN_l%num_time_labels
        call add_new_field_address(ifld, jcou, n_scalar, ist_Comp)
        call add_new_field_address(ifld, jcou, n_scalar, ist_Clength_l)
        call add_new_field_address(ifld, jcou, n_scalar, ist_Clength_m)
      end if
!
      sph_OUT1%nfield_sph_spec = ifld
      sph_OUT1%ntot_sph_spec = jcou
      sph_OUT1%num_labels                                               &
     &        = sph_OUT1%ntot_sph_spec + sph_OUT1%num_time_labels
!
      call alloc_sph_espec_name(sph_OUT1)
      call alloc_sph_spectr_data(izero, sph_OUT1)
!
      sph_OUT1%ene_sph_spec_name(1:sph_OUT1%num_time_labels)            &
     &         = sph_IN_l%ene_sph_spec_name(1:sph_OUT1%num_time_labels)
!
      ifld = 0
      jcou = sph_OUT1%num_time_labels
      kcou = sph_IN_l%num_time_labels
      if(els_dat%irms_KE .ge. 3) then
        write(sph_OUT1%ene_sph_spec_name(ist_Kene+jcou  ),'(a)')        &
     &    trim(sph_IN_l%ene_sph_spec_name(els_dat%irms_KE+kcou-2))
        write(sph_OUT1%ene_sph_spec_name(ist_Kene+jcou+1),'(a)')        &
     &    trim(sph_IN_l%ene_sph_spec_name(els_dat%irms_KE+kcou-1))
        write(sph_OUT1%ene_sph_spec_name(ist_Kene+jcou+2),'(a)')        &
     &    trim(sph_IN_l%ene_sph_spec_name(els_dat%irms_KE+kcou  ))
        sph_OUT1%ncomp_sph_spec(ifld+1) = 3
!
        write(sph_OUT1%ene_sph_spec_name(ist_Re+jcou),'(a)')            &
     &    'Re'
        sph_OUT1%ncomp_sph_spec(ifld+2) = 1
!
        write(sph_OUT1%ene_sph_spec_name(ist_ulength_l+jcou  ),'(a,a)') &
     &    trim(sph_IN_l%ene_sph_spec_name(els_dat%irms_KE+kcou-2)),     &
     &    '_scale_l'
        write(sph_OUT1%ene_sph_spec_name(ist_ulength_l+jcou+1),'(a,a)') &
     &    trim(sph_IN_l%ene_sph_spec_name(els_dat%irms_KE+kcou-1)),     &
     &   '_scale_l'
        write(sph_OUT1%ene_sph_spec_name(ist_ulength_l+jcou+2),'(a,a)') &
     &    trim(sph_IN_l%ene_sph_spec_name(els_dat%irms_KE+kcou  )),     &
     &   '_scale_l'
        sph_OUT1%ncomp_sph_spec(ifld+3) = 3
!
        write(sph_OUT1%ene_sph_spec_name(ist_ulength_m+jcou  ),'(a,a)') &
     &    trim(sph_IN_l%ene_sph_spec_name(els_dat%irms_KE+kcou-2)),     &
     &    '_scale_m'
        write(sph_OUT1%ene_sph_spec_name(ist_ulength_m+jcou+1),'(a,a)') &
     &    trim(sph_IN_l%ene_sph_spec_name(els_dat%irms_KE+kcou-1)),     &
     &   '_scale_m'
        write(sph_OUT1%ene_sph_spec_name(ist_ulength_m+jcou+2),'(a,a)') &
     &    trim(sph_IN_l%ene_sph_spec_name(els_dat%irms_KE+kcou  )),     &
     &   '_scale_m'
        sph_OUT1%ncomp_sph_spec(ifld+4) = 3
        ifld = ifld + 4
      end if
      if(els_dat%irms_ME .ge. 3) then
        write(sph_OUT1%ene_sph_spec_name(ist_MEne+jcou  ),'(a)')        &
     &    trim(sph_IN_l%ene_sph_spec_name(els_dat%irms_ME+kcou-2))
        write(sph_OUT1%ene_sph_spec_name(ist_MEne+jcou+1),'(a)')        &
     &    trim(sph_IN_l%ene_sph_spec_name(els_dat%irms_ME+kcou-1))
        write(sph_OUT1%ene_sph_spec_name(ist_MEne+jcou+2),'(a)')        &
     &    trim(sph_IN_l%ene_sph_spec_name(els_dat%irms_ME+kcou  ))
        sph_OUT1%ncomp_sph_spec(ifld+1) = 3
!
        write(sph_OUT1%ene_sph_spec_name(ist_Elsasser+jcou),'(a)')      &
     &    'Elsasser'
        sph_OUT1%ncomp_sph_spec(ifld+2) = 1
!
        write(sph_OUT1%ene_sph_spec_name(ist_Blength_l+jcou  ),'(a,a)') &
     &    trim(sph_IN_l%ene_sph_spec_name(els_dat%irms_ME+kcou-2)),     &
     &    '_scale_l'
        write(sph_OUT1%ene_sph_spec_name(ist_Blength_l+jcou+1),'(a,a)') &
     &    trim(sph_IN_l%ene_sph_spec_name(els_dat%irms_ME+kcou-1)),     &
     &    '_scale_l'
        write(sph_OUT1%ene_sph_spec_name(ist_Blength_l+jcou+2),'(a,a)') &
     &    trim(sph_IN_l%ene_sph_spec_name(els_dat%irms_ME+kcou  )),     &
     &    '_scale_l'
        sph_OUT1%ncomp_sph_spec(ifld+3) = 3
!
        write(sph_OUT1%ene_sph_spec_name(ist_Blength_m+jcou  ),'(a,a)') &
     &    trim(sph_IN_l%ene_sph_spec_name(els_dat%irms_ME+kcou-2)),     &
     &    '_scale_m'
        write(sph_OUT1%ene_sph_spec_name(ist_Blength_m+jcou+1),'(a,a)') &
     &    trim(sph_IN_l%ene_sph_spec_name(els_dat%irms_ME+kcou-1)),     &
     &    '_scale_m'
        write(sph_OUT1%ene_sph_spec_name(ist_Blength_m+jcou+2),'(a,a)') &
     &    trim(sph_IN_l%ene_sph_spec_name(els_dat%irms_ME+kcou  )),     &
     &    '_scale_m'
        sph_OUT1%ncomp_sph_spec(ifld+4) = 3
        ifld = ifld + 4
      end if
      if(els_dat%irms_KE .ge. 3 .and. els_dat%irms_ME .ge. 3) then
        write(sph_OUT1%ene_sph_spec_name(ist_Rm+jcou),'(a)')            &
     &    'Rm'
        sph_OUT1%ncomp_sph_spec(ifld+1) = 1
        write(sph_OUT1%ene_sph_spec_name(ist_dyn_Els+jcou),'(a)')       &
     &    'Dynamic_Elsasser'
        sph_OUT1%ncomp_sph_spec(ifld+2) = 1
        ifld = ifld + 2
      end if
      if(els_dat%irms_T .ge. 1) then
        write(sph_OUT1%ene_sph_spec_name(ist_Temp+jcou),'(a,a)')        &
     &    trim(sph_IN_l%ene_sph_spec_name(els_dat%irms_T+kcou)),        &
     &    '_part'
        sph_OUT1%ncomp_sph_spec(ifld+1) = 1
        write(sph_OUT1%ene_sph_spec_name(ist_Tlength_l+jcou),'(a,a)')   &
     &    trim(sph_IN_l%ene_sph_spec_name(els_dat%irms_T+kcou)),        &
     &    '_scale_l'
        sph_OUT1%ncomp_sph_spec(ifld+2) = 1
        write(sph_OUT1%ene_sph_spec_name(ist_Tlength_m+jcou),'(a,a)')   &
     &    trim(sph_IN_l%ene_sph_spec_name(els_dat%irms_T+kcou)),        &
     &    '_scale_m'
        sph_OUT1%ncomp_sph_spec(ifld+3) = 1
        ifld = ifld + 3
      end if
      if(els_dat%irms_C .ge. 1) then
        write(sph_OUT1%ene_sph_spec_name(ist_Comp+jcou),'(a,a)')        &
     &    trim(sph_IN_l%ene_sph_spec_name(els_dat%irms_C+kcou)),        &
     &    '_part'
        sph_OUT1%ncomp_sph_spec(ifld+1) = 1
        write(sph_OUT1%ene_sph_spec_name(ist_Clength_l+jcou),'(a,a)')   &
     &    trim(sph_IN_l%ene_sph_spec_name(els_dat%irms_C+kcou)),        &
     &    '_scale_l'
        sph_OUT1%ncomp_sph_spec(ifld+2) = 1
        write(sph_OUT1%ene_sph_spec_name(ist_Clength_m+jcou),'(a,a)')   &
     &    trim(sph_IN_l%ene_sph_spec_name(els_dat%irms_C+kcou)),        &
     &    '_scale_m'
        sph_OUT1%ncomp_sph_spec(ifld+3) = 1
        ifld = ifld + 3
      end if
!
!
      file_name = add_dat_extension(els_dat%elsasser_file_prefix)
      write(*,*) 'Save Elsasser number into  ', trim(file_name)
      open(id_file_lscale, file=file_name)
      call select_output_sph_pwr_head                                   &
     &   (id_file_lscale, vol_ave_on, sph_OUT1)
!
      icou = 0
      ist_true = -1
      write(*,'(a5,i12,a30,i12)',advance="NO")                          &
     &       'step= ', sph_IN_l%i_step,                                 &
     &       ' averaging finished. Count=  ', icou
      do
        call select_input_sph_series_data(id_file_rms_l, flag_gzip_l,   &
     &      els_dat%flag_old_spectr_data, spectr_on, vol_ave_on,        &
     &      sph_IN_l, zbuf_l, ierr)
        call select_input_sph_series_data(id_file_rms_m, flag_gzip_m,   &
     &      els_dat%flag_old_spectr_data, spectr_on, vol_ave_on,        &
     &      sph_IN_m, zbuf_m, ierr)
        if(ierr .gt. 0) go to 99
!
        if (sph_IN_l%time .ge. els_dat%start_time) then
          call copy_read_ene_step_data(sph_IN_l, sph_OUT1)
          icou = icou + 1
!
          if(els_dat%irms_KE .ge. 3) then
            call sum_ene_spectr_3(sph_IN_l, els_dat%irms_KE,            &
     &                            sph_OUT1%spectr_IO(ist_KEne,0,1))
            call uli_sph_length_scale_3(sph_IN_l, els_dat%irms_KE,      &
     &          sph_OUT1%spectr_IO(ist_ulength_l,0,1))
            call uli_sph_length_scale_3(sph_IN_m, els_dat%irms_KE,      &
     &          sph_OUT1%spectr_IO(ist_ulength_m,0,1))
!
            sph_OUT1%spectr_IO(ist_Re,0,1)                              &
     &          = sqrt(two * sph_OUT1%spectr_IO(ist_KEne+2,0,1) )
          end if
          if(els_dat%irms_ME .ge. 3) then
            call sum_ene_spectr_3(sph_IN_l, els_dat%irms_ME,            &
     &                            sph_OUT1%spectr_IO(ist_MEne,0,1))
            sph_OUT1%spectr_IO(ist_MEne:ist_MEne+2,0,1)                 &
     &          = sph_OUT1%spectr_IO(ist_MEne:ist_MEne+2,0,1)           &
     &           * els_dat%ME_scale
!
            call uli_sph_length_scale_3(sph_IN_l, els_dat%irms_ME,      &
     &          sph_OUT1%spectr_IO(ist_Blength_l,0,1))
            call uli_sph_length_scale_3(sph_IN_m, els_dat%irms_ME,      &
     &          sph_OUT1%spectr_IO(ist_Blength_m,0,1))
!
            sph_OUT1%spectr_IO(ist_Elsasser,0,1)                        &
     &          =  sph_OUT1%spectr_IO(ist_MEne+2,0,1)                   &
     &           * els_dat%coef_elsasser
          end if
          if(els_dat%irms_KE .ge. 3 .and. els_dat%irms_ME .ge. 3) then
            sph_OUT1%spectr_IO(ist_Rm,0,1)                              &
     &          = sqrt(two * sph_OUT1%spectr_IO(ist_KEne+2,0,1) )       &
     &           * els_dat%mag_Re_ratio
!
!            lscale_b = (half*pi*Lscale)                                &
!     &               / sqrt(sph_OUT1%spectr_IO(ist_Blength_l+2,0,1)**2 &
!     &                    + sph_OUT1%spectr_IO(ist_Blength_m+2,0,1)**2)
!            sph_OUT1%spectr_IO(ist_dyn_Els,0,1)                        &
!     &          = sph_OUT1%spectr_IO(ist_Elsasser,0,1) * Lscale        &
!     &               / sqrt(sph_OUT1%spectr_IO(ist_Blength_l+2,0,1)**2 &
!     &                    + sph_OUT1%spectr_IO(ist_Blength_m+2,0,1)**2)
!     &           / (sph_OUT1%spectr_IO(ist_Rm,0,1)  * lscale_b)
            sph_OUT1%spectr_IO(ist_dyn_Els,0,1)                        &
     &          = sph_OUT1%spectr_IO(ist_Elsasser,0,1)                 &
     &            * sqrt(sph_OUT1%spectr_IO(ist_Blength_l+2,0,1)**2    &
     &                 + sph_OUT1%spectr_IO(ist_Blength_m+2,0,1)**2)   &
     &           / (sph_OUT1%spectr_IO(ist_Rm,0,1) * half*pi)
          end if
          if(els_dat%irms_T .ge. 1) then
            sph_OUT1%spectr_IO(ist_Temp,  0,1)                          &
     &                = sum_ene_spectr(els_dat%irms_T,                  &
     &                                 sph_IN_l%ltr_sph,                &
     &                                 sph_IN_l%ntot_sph_spec,          &
     &                                 sph_IN_l%spectr_IO(1,0,1))
            sph_OUT1%spectr_IO(ist_Tlength_l,  0,1)                     &
     &          = uli_sph_length_scale(els_dat%irms_T,                  &
     &                                 sph_IN_l%ltr_sph,                &
     &                                 sph_IN_l%ntot_sph_spec,          &
     &                                 sph_IN_l%spectr_IO(1,0,1))
            sph_OUT1%spectr_IO(ist_Tlength_m,  0,1)                     &
     &          = uli_sph_length_scale(els_dat%irms_T,                  &
     &                                 sph_IN_m%ltr_sph,                &
     &                                 sph_IN_m%ntot_sph_spec,          &
     &                                 sph_IN_m%spectr_IO(1,0,1))
          end if
          if(els_dat%irms_C .ge. 1) then
            sph_OUT1%spectr_IO(ist_Comp,  0,1)                          &
     &                = sum_ene_spectr(els_dat%irms_C,                  &
     &                                 sph_IN_l%ltr_sph,                &
     &                                 sph_IN_l%ntot_sph_spec,          &
     &                                 sph_IN_l%spectr_IO(1,0,1))
            sph_OUT1%spectr_IO(ist_Clength_l,  0,1)                     &
     &          = uli_sph_length_scale(els_dat%irms_C,                  &
     &                                 sph_IN_l%ltr_sph,                &
     &                                 sph_IN_l%ntot_sph_spec,          &
     &                                 sph_IN_l%spectr_IO(1,0,1))
            sph_OUT1%spectr_IO(ist_Clength_m,  0,1)                     &
     &          = uli_sph_length_scale(els_dat%irms_C,                  &
     &                                 sph_IN_m%ltr_sph,                &
     &                                 sph_IN_m%ntot_sph_spec,          &
     &                                 sph_IN_m%spectr_IO(1,0,1))
          end if
!
!
!
          call select_output_sph_series_data                            &
     &       (id_file_lscale, spectr_off, vol_ave_on, sph_OUT1)
        end if
!
        write(*,'(59a1,a5,i12,a30,i12)',advance="NO") (char(8),i=1,59), &
     &       'step= ', sph_IN_l%i_step,                                 &
     &       ' evaluation finished. Count=  ', icou
        if (sph_IN_l%time .ge. els_dat%end_time) exit
      end do
!
   99 continue
      write(*,*)
      call sel_close_sph_monitor_file(id_file_rms_l,                    &
     &                                flag_gzip_l, zbuf_l)
      call sel_close_sph_monitor_file(id_file_rms_m,                    &
     &                                flag_gzip_m, zbuf_m)
      close(id_file_lscale)
!
!
      call dealloc_sph_espec_data(sph_IN_l)
      call dealloc_sph_espec_data(sph_IN_m)
      call dealloc_sph_espec_data(sph_OUT1)
!
      end subroutine sph_dynamic_elsasser_by_spectr
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine sum_ene_spectr_3(sph_IN, irms_end, ene_3)
!
      type(read_sph_spectr_data), intent(in) :: sph_IN
      integer(kind = kint), intent(in) :: irms_end
      real(kind = kreal), intent(inout) :: ene_3(3)
!
      ene_3(1) = sum_ene_spectr(irms_end-2, sph_IN%ltr_sph,             &
     &           sph_IN%ntot_sph_spec, sph_IN%spectr_IO(1,0,1))
      ene_3(2) = sum_ene_spectr(irms_end-1, sph_IN%ltr_sph,             &
     &           sph_IN%ntot_sph_spec, sph_IN%spectr_IO(1,0,1))
      ene_3(3) = sum_ene_spectr(irms_end, sph_IN%ltr_sph,               &
     &           sph_IN%ntot_sph_spec, sph_IN%spectr_IO(1,0,1))
!
      end subroutine sum_ene_spectr_3
!
!   --------------------------------------------------------------------
!
      subroutine uli_sph_length_scale_3(sph_IN, irms_end, lscale_3)
!
      type(read_sph_spectr_data), intent(in) :: sph_IN
      integer(kind = kint), intent(in) :: irms_end
      real(kind = kreal), intent(inout) :: lscale_3(3)
!
      lscale_3(1) = uli_sph_length_scale(irms_end-2, sph_IN%ltr_sph,    &
     &           sph_IN%ntot_sph_spec, sph_IN%spectr_IO(1,0,1))
      lscale_3(2) = uli_sph_length_scale(irms_end-1, sph_IN%ltr_sph,    &
     &           sph_IN%ntot_sph_spec, sph_IN%spectr_IO(1,0,1))
      lscale_3(3) = uli_sph_length_scale(irms_end, sph_IN%ltr_sph,      &
     &           sph_IN%ntot_sph_spec, sph_IN%spectr_IO(1,0,1))
!
      end subroutine uli_sph_length_scale_3
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      real(kind = kreal) function sum_ene_spectr                        &
     &                          (irms, ltr_sph, ncomp, spectr_l)
!
      integer(kind = kint), intent(in) :: ltr_sph, ncomp, irms
      real(kind = kreal), intent(in) :: spectr_l(ncomp, 0:ltr_sph)
!
      integer(kind = kint) :: l
!
!
      sum_ene_spectr = 0.0d0
      do l = 1, ltr_sph
        sum_ene_spectr = sum_ene_spectr + spectr_l(irms,l)
      end do
!
      end function sum_ene_spectr
!
!   --------------------------------------------------------------------
!
      real(kind = kreal) function uli_sph_length_scale                  &
     &                          (irms, ltr_sph, ncomp, spectr_l)
!
      integer(kind = kint), intent(in) :: ltr_sph, ncomp, irms
      real(kind = kreal), intent(in) :: spectr_l(ncomp, 0:ltr_sph)
!
      real(kind = kreal) :: total_msq, spec_times_l
!
      integer(kind = kint) :: l
!
!
      total_msq = 0.0d0
      spec_times_l = 0.0d0
!
      do l = 1, ltr_sph
        total_msq = total_msq + spectr_l(irms,l)
        spec_times_l = spec_times_l + spectr_l(irms,l) * dble(l)
      end do
!
      if(total_msq .le. 0.0d0) then
        uli_sph_length_scale = 0.0d0
      else
        uli_sph_length_scale = spec_times_l / total_msq
      end if
!
      end function uli_sph_length_scale
!
!   --------------------------------------------------------------------
!
      subroutine add_new_field_address(ifld, icomp, ncomp, i_start)
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(inout) :: ifld, icomp, i_start
!
      i_start = icomp + 1
      icomp = icomp + ncomp
      ifld = ifld +   1
      end subroutine add_new_field_address
!
!   --------------------------------------------------------------------
!
      end module m_sph_dynamic_elsasser
