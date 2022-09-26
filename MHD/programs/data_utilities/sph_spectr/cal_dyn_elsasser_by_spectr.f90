!>@file   cal_dyn_elsasser_by_spectr.f90
!!        module cal_dyn_elsasser_by_spectr
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Obtain lengh scale from spherical harmonics spectrum data
!!
!!@verbatim
!!      subroutine set_dynamic_elsasser_name(sph_IN, iels, els_dat,     &
!!     &          num_labels, nfield_sph_spec, num_time_labels,         &
!!     &          ncomp_sph_spec, ene_sph_spec_name)
!!        type(read_sph_spectr_data), intent(in) :: sph_IN
!!        type(dyn_elsasser_address), intent(in) :: iels
!!        type(sph_dyn_elsasser_data), intent(in) :: els_dat
!!        integer(kind = kint), intent(in) :: num_labels, nfield_sph_spec
!!        integer(kind = kint), intent(in) :: num_time_labels
!!        integer(kind = kint), intent(inout)                           &
!!     &                        :: ncomp_sph_spec(nfield_sph_spec)
!!        character(len = kchara), intent(inout)                        &
!!     &                        :: ene_sph_spec_name(num_labels)
!!      subroutine cal_dynamic_elsasser_by_spectr                       &
!!     &         (sph_IN_l, sph_IN_m, iels, els_dat,                    &
!!     &          ntot_sph_spec, elsassers)
!!        type(read_sph_spectr_data), intent(in) :: sph_IN_l, sph_IN_m
!!        type(dyn_elsasser_address), intent(in) :: iels
!!        type(sph_dyn_elsasser_data), intent(in) :: els_dat
!!        integer(kind = kint), intent(in) :: ntot_sph_spec
!!        real(kind = kreal), intent(inout) :: elsassers(ntot_sph_spec)
!!@endverbatim
      module cal_dyn_elsasser_by_spectr
!
      use m_precision
      use m_constants
      use m_phys_constants
      use t_dyn_elsasser_address
      use t_ctl_param_dyn_elsasser
!
      implicit none
!
      real(kind = kreal), parameter, private :: pi = four * atan(one)
      real(kind = kreal), parameter, private :: Lscale = 1.0
!
      private :: sum_ene_spectr_3, uli_sph_length_scale_3
      private :: sum_ene_spectr,   uli_sph_length_scale
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_dynamic_elsasser_name(sph_IN, iels, els_dat,       &
     &          num_labels, nfield_sph_spec, num_time_labels,           &
     &          ncomp_sph_spec, ene_sph_spec_name)
!
      type(read_sph_spectr_data), intent(in) :: sph_IN
      type(dyn_elsasser_address), intent(in) :: iels
      type(sph_dyn_elsasser_data), intent(in) :: els_dat
      integer(kind = kint), intent(in) :: num_labels, nfield_sph_spec
      integer(kind = kint), intent(in) :: num_time_labels
!
      integer(kind = kint), intent(inout)                               &
     &                        :: ncomp_sph_spec(nfield_sph_spec)
      character(len = kchara), intent(inout)                            &
     &                        :: ene_sph_spec_name(num_labels)
!
      integer(kind = kint) :: jcou, kcou, ifld
!
!
      ene_sph_spec_name(1:num_time_labels)                              &
     &         = sph_IN%ene_sph_spec_name(1:num_time_labels)
!
      ifld = 0
      jcou = num_time_labels
      kcou = sph_IN%num_time_labels
      if(els_dat%irms_KE .ge. 3) then
        write(ene_sph_spec_name(iels%ist_Kene+jcou  ),'(a)')            &
     &    trim(sph_IN%ene_sph_spec_name(els_dat%irms_KE+kcou-2))
        write(ene_sph_spec_name(iels%ist_Kene+jcou+1),'(a)')            &
     &    trim(sph_IN%ene_sph_spec_name(els_dat%irms_KE+kcou-1))
        write(ene_sph_spec_name(iels%ist_Kene+jcou+2),'(a)')            &
     &    trim(sph_IN%ene_sph_spec_name(els_dat%irms_KE+kcou  ))
        ncomp_sph_spec(ifld+1) = 3
!
        write(ene_sph_spec_name(iels%ist_Re+jcou),'(a)') 'Re'
        ncomp_sph_spec(ifld+2) = 1
!
        write(ene_sph_spec_name(iels%ist_ulength_l+jcou  ), '(a,a)')    &
     &    trim(sph_IN%ene_sph_spec_name(els_dat%irms_KE+kcou-2)),       &
     &    '_scale_l'
        write(ene_sph_spec_name(iels%ist_ulength_l+jcou+1), '(a,a)')    &
     &    trim(sph_IN%ene_sph_spec_name(els_dat%irms_KE+kcou-1)),       &
     &   '_scale_l'
        write(ene_sph_spec_name(iels%ist_ulength_l+jcou+2), '(a,a)')    &
     &    trim(sph_IN%ene_sph_spec_name(els_dat%irms_KE+kcou  )),       &
     &   '_scale_l'
        ncomp_sph_spec(ifld+3) = 3
!
        write(ene_sph_spec_name(iels%ist_ulength_m+jcou  ), '(a,a)')    &
     &    trim(sph_IN%ene_sph_spec_name(els_dat%irms_KE+kcou-2)),       &
     &    '_scale_m'
        write(ene_sph_spec_name(iels%ist_ulength_m+jcou+1), '(a,a)')    &
     &    trim(sph_IN%ene_sph_spec_name(els_dat%irms_KE+kcou-1)),       &
     &   '_scale_m'
        write(ene_sph_spec_name(iels%ist_ulength_m+jcou+2), '(a,a)')    &
     &    trim(sph_IN%ene_sph_spec_name(els_dat%irms_KE+kcou  )),       &
     &   '_scale_m'
        ncomp_sph_spec(ifld+4) = 3
        ifld = ifld + 4
      end if
      if(els_dat%irms_ME .ge. 3) then
        write(ene_sph_spec_name(iels%ist_MEne+jcou  ),'(a)')            &
     &    trim(sph_IN%ene_sph_spec_name(els_dat%irms_ME+kcou-2))
        write(ene_sph_spec_name(iels%ist_MEne+jcou+1),'(a)')            &
     &    trim(sph_IN%ene_sph_spec_name(els_dat%irms_ME+kcou-1))
        write(ene_sph_spec_name(iels%ist_MEne+jcou+2),'(a)')            &
     &    trim(sph_IN%ene_sph_spec_name(els_dat%irms_ME+kcou  ))
        ncomp_sph_spec(ifld+1) = 3
!
        write(ene_sph_spec_name(iels%ist_Elsasser+jcou), '(a)')         &
     &                                                     'Elsasser'
        ncomp_sph_spec(ifld+2) = 1
!
        write(ene_sph_spec_name(iels%ist_Blength_l+jcou  ), '(a,a)')    &
     &    trim(sph_IN%ene_sph_spec_name(els_dat%irms_ME+kcou-2)),       &
     &    '_scale_l'
        write(ene_sph_spec_name(iels%ist_Blength_l+jcou+1), '(a,a)')    &
     &    trim(sph_IN%ene_sph_spec_name(els_dat%irms_ME+kcou-1)),       &
     &    '_scale_l'
        write(ene_sph_spec_name(iels%ist_Blength_l+jcou+2), '(a,a)')    &
     &    trim(sph_IN%ene_sph_spec_name(els_dat%irms_ME+kcou  )),       &
     &    '_scale_l'
        ncomp_sph_spec(ifld+3) = 3
!
        write(ene_sph_spec_name(iels%ist_Blength_m+jcou  ), '(a,a)')    &
     &    trim(sph_IN%ene_sph_spec_name(els_dat%irms_ME+kcou-2)),       &
     &    '_scale_m'
        write(ene_sph_spec_name(iels%ist_Blength_m+jcou+1), '(a,a)')    &
     &    trim(sph_IN%ene_sph_spec_name(els_dat%irms_ME+kcou-1)),       &
     &    '_scale_m'
        write(ene_sph_spec_name(iels%ist_Blength_m+jcou+2), '(a,a)')    &
     &    trim(sph_IN%ene_sph_spec_name(els_dat%irms_ME+kcou  )),       &
     &    '_scale_m'
        ncomp_sph_spec(ifld+4) = 3
        ifld = ifld + 4
      end if
      if(els_dat%irms_KE .ge. 3 .and. els_dat%irms_ME .ge. 3) then
        write(ene_sph_spec_name(iels%ist_Rm+jcou),'(a)') 'Rm'
        ncomp_sph_spec(ifld+1) = 1
        write(ene_sph_spec_name(iels%ist_dyn_Els+jcou),'(a)')           &
     &                   'Dynamic_Elsasser'
        ncomp_sph_spec(ifld+2) = 1
        ifld = ifld + 2
      end if
      if(els_dat%irms_T .ge. 1) then
        write(ene_sph_spec_name(iels%ist_Temp+jcou), '(a,a)')           &
     &    trim(sph_IN%ene_sph_spec_name(els_dat%irms_T+kcou)), '_part'
        ncomp_sph_spec(ifld+1) = 1
        write(ene_sph_spec_name(iels%ist_Tlength_l+jcou), '(a,a)')      &
     &    trim(sph_IN%ene_sph_spec_name(els_dat%irms_T+kcou)),          &
     &    '_scale_l'
        ncomp_sph_spec(ifld+2) = 1
        write(ene_sph_spec_name(iels%ist_Tlength_m+jcou), '(a,a)')      &
     &    trim(sph_IN%ene_sph_spec_name(els_dat%irms_T+kcou)),          &
     &    '_scale_m'
        ncomp_sph_spec(ifld+3) = 1
        ifld = ifld + 3
      end if
      if(els_dat%irms_C .ge. 1) then
        write(ene_sph_spec_name(iels%ist_Comp+jcou),'(a,a)')            &
     &    trim(sph_IN%ene_sph_spec_name(els_dat%irms_C+kcou)), '_part'
        ncomp_sph_spec(ifld+1) = 1
        write(ene_sph_spec_name(iels%ist_Clength_l+jcou), '(a,a)')      &
     &    trim(sph_IN%ene_sph_spec_name(els_dat%irms_C+kcou)),          &
     &    '_scale_l'
        ncomp_sph_spec(ifld+2) = 1
        write(ene_sph_spec_name(iels%ist_Clength_m+jcou), '(a,a)')      &
     &    trim(sph_IN%ene_sph_spec_name(els_dat%irms_C+kcou)),          &
     &    '_scale_m'
        ncomp_sph_spec(ifld+3) = 1
        ifld = ifld + 3
      end if
!
      end subroutine set_dynamic_elsasser_name
!
!   --------------------------------------------------------------------
!
      subroutine cal_dynamic_elsasser_by_spectr                         &
     &         (sph_IN_l, sph_IN_m, iels, els_dat,                      &
     &          ntot_sph_spec, elsassers)
!
      type(read_sph_spectr_data), intent(in) :: sph_IN_l, sph_IN_m
      type(dyn_elsasser_address), intent(in) :: iels
      type(sph_dyn_elsasser_data), intent(in) :: els_dat
      integer(kind = kint), intent(in) :: ntot_sph_spec
      real(kind = kreal), intent(inout) :: elsassers(ntot_sph_spec)
!
      real(kind = kreal) :: lscale_b
!
!
      if(els_dat%irms_KE .ge. 3) then
        call sum_ene_spectr_3(sph_IN_l, els_dat%irms_KE,                &
     &      elsassers(iels%ist_KEne))
        call uli_sph_length_scale_3(sph_IN_l, els_dat%irms_KE,          &
     &      elsassers(iels%ist_ulength_l))
        call uli_sph_length_scale_3(sph_IN_m, els_dat%irms_KE,          &
     &      elsassers(iels%ist_ulength_m))
!
        elsassers(iels%ist_Re) = sqrt(two*elsassers(iels%ist_KEne+2))
      end if
!
      if(els_dat%irms_ME .ge. 3) then
        call sum_ene_spectr_3(sph_IN_l, els_dat%irms_ME,                &
     &      elsassers(iels%ist_MEne))
        elsassers(iels%ist_MEne:iels%ist_MEne+2)                        &
     &    = elsassers(iels%ist_MEne:iels%ist_MEne+2)                    &
     &       * els_dat%ME_scale
!
        call uli_sph_length_scale_3(sph_IN_l, els_dat%irms_ME,          &
     &      elsassers(iels%ist_Blength_l))
        call uli_sph_length_scale_3(sph_IN_m, els_dat%irms_ME,          &
     &      elsassers(iels%ist_Blength_m))
!
        elsassers(iels%ist_Elsasser)                                    &
     &      = two * elsassers(iels%ist_MEne+2) * els_dat%coef_elsasser
      end if
!
      if(els_dat%irms_KE .ge. 3 .and. els_dat%irms_ME .ge. 3) then
        elsassers(iels%ist_Rm)                                          &
     &      = sqrt(two * elsassers(iels%ist_KEne+2) )                   &
     &       * els_dat%mag_Re_ratio
!
        lscale_b = (half*pi*Lscale)                                     &
     &      / sqrt(elsassers(iels%ist_Blength_l+2)**2                   &
     &          + elsassers(iels%ist_Blength_m+2)**2)
        elsassers(iels%ist_dyn_Els)                                     &
     &     = elsassers(iels%ist_Elsasser) * Lscale                      &
     &      / sqrt(elsassers(iels%ist_Blength_l+2)**2                   &
     &          + elsassers(iels%ist_Blength_m+2)**2)                   &
     &       / (elsassers(iels%ist_Rm)  * lscale_b)
        elsassers(iels%ist_dyn_Els)                                     &
     &     = elsassers(iels%ist_Elsasser)                               &
     &      * sqrt(elsassers(iels%ist_Blength_l+2)**2                   &
     &          + elsassers(iels%ist_Blength_m+2)**2)                   &
     &       / (elsassers(iels%ist_Rm) * half*pi)
      end if
      if(els_dat%irms_T .ge. 1) then
        elsassers(iels%ist_Temp)                                        &
     &            = sum_ene_spectr(els_dat%irms_T,                      &
     &                             sph_IN_l%ltr_sph,                    &
     &                             sph_IN_l%ntot_sph_spec,              &
     &                             sph_IN_l%spectr_IO(1,0,1))
        elsassers(iels%ist_Tlength_l)                                   &
     &      = uli_sph_length_scale(els_dat%irms_T,                      &
     &                             sph_IN_l%ltr_sph,                    &
     &                             sph_IN_l%ntot_sph_spec,              &
     &                             sph_IN_l%spectr_IO(1,0,1))
        elsassers(iels%ist_Tlength_m)                                   &
     &      = uli_sph_length_scale(els_dat%irms_T,                      &
     &                             sph_IN_m%ltr_sph,                    &
     &                             sph_IN_m%ntot_sph_spec,              &
     &                             sph_IN_m%spectr_IO(1,0,1))
      end if
      if(els_dat%irms_C .ge. 1) then
        elsassers(iels%ist_Comp)                                        &
     &          = sum_ene_spectr(els_dat%irms_C,                        &
     &                           sph_IN_l%ltr_sph,                      &
     &                           sph_IN_l%ntot_sph_spec,                &
     &                           sph_IN_l%spectr_IO(1,0,1))
        elsassers(iels%ist_Clength_l)                                   &
     &      = uli_sph_length_scale(els_dat%irms_C,                      &
     &                             sph_IN_l%ltr_sph,                    &
     &                             sph_IN_l%ntot_sph_spec,              &
     &                             sph_IN_l%spectr_IO(1,0,1))
        elsassers(iels%ist_Clength_m)                                   &
     &      = uli_sph_length_scale(els_dat%irms_C,                      &
     &                             sph_IN_m%ltr_sph,                    &
     &                             sph_IN_m%ntot_sph_spec,              &
     &                             sph_IN_m%spectr_IO(1,0,1))
      end if
!
      end subroutine cal_dynamic_elsasser_by_spectr
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
      end module cal_dyn_elsasser_by_spectr
