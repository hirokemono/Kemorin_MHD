!>@file   t_dyn_elsasser_address.f90
!!        module t_dyn_elsasser_address
!!
!! @author H. Matsui
!! @date   Programmed in  Sep., 2022
!!
!
!> @brief Addresses for dynamic Elsasser number program
!!
!!@verbatim
!!      subroutine check_dynamic_elsasser_address(els_dat, iels)
!!        type(sph_dyn_elsasser_data), intent(in) :: els_dat
!!        type(dyn_elsasser_address), intent(in) :: iels
!!      subroutine set_dynamic_elsasser_address                         &
!!     &         (els_dat, iels, nfield_sph_spec, ntot_sph_spec)
!!        type(sph_dyn_elsasser_data), intent(in) :: els_dat
!!        type(dyn_elsasser_address), intent(inout) :: iels
!!        integer(kind = kint), intent(inout) :: nfield_sph_spec
!!        integer(kind = kint), intent(inout) :: ntot_sph_spec
!!@endverbatim
!
      module t_dyn_elsasser_address
!
      use m_precision
      use m_constants
      use t_ctl_param_dyn_elsasser
!
      implicit none
!
      type dyn_elsasser_address
        integer(kind = kint) :: ist_KEne =  0
        integer(kind = kint) :: ist_MEne =  0
        integer(kind = kint) :: ist_Temp =  0
        integer(kind = kint) :: ist_Comp =  0
!
        integer(kind = kint) :: ist_Re =  0
        integer(kind = kint) :: ist_Rm =  0
        integer(kind = kint) :: ist_Elsasser = 0
        integer(kind = kint) :: ist_dyn_Els =  0
!
        integer(kind = kint) :: ist_ulength_l =  0
        integer(kind = kint) :: ist_Blength_l =  0
        integer(kind = kint) :: ist_Tlength_l =  0
        integer(kind = kint) :: ist_Clength_l =  0
!
        integer(kind = kint) :: ist_ulength_m =  0
        integer(kind = kint) :: ist_Blength_m =  0
        integer(kind = kint) :: ist_Tlength_m =  0
        integer(kind = kint) :: ist_Clength_m =  0
      end type dyn_elsasser_address
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine check_dynamic_elsasser_address(els_dat, iels)
!
      type(sph_dyn_elsasser_data), intent(in) :: els_dat
      type(dyn_elsasser_address), intent(in) :: iels
!
!
      write(*,*) 'els_dat%irms_KE', els_dat%irms_KE
      write(*,*) 'els_dat%irms_ME', els_dat%irms_ME
      write(*,*) 'els_dat%irms_T', els_dat%irms_T
      write(*,*) 'els_dat%irms_C', els_dat%irms_C
!
      write(*,*) 'ist_KEne',      iels%ist_KEne
      write(*,*) 'ist_ulength_l', iels%ist_ulength_l
      write(*,*) 'ist_ulength_m', iels%ist_ulength_m
      write(*,*) 'ist_Elsasser',   iels%ist_Elsasser
      write(*,*) 'ist_Blength_l',  iels%ist_Blength_l
      write(*,*) 'ist_Blength_m',  iels%ist_Blength_m
      write(*,*) 'ist_Rm',         iels%ist_Rm
      write(*,*) 'ist_dyn_Els',    iels%ist_dyn_Els
      write(*,*) 'ist_Temp',       iels%ist_Temp
      write(*,*) 'ist_Tlength_l',  iels%ist_Tlength_l
      write(*,*) 'ist_Tlength_m',  iels%ist_Tlength_m
      write(*,*) 'ist_Comp',       iels%ist_Comp
      write(*,*) 'ist_Clength_l',  iels%ist_Clength_l
      write(*,*) 'ist_Clength_m',  iels%ist_Clength_m
!
      end subroutine check_dynamic_elsasser_address
!
!   --------------------------------------------------------------------
!
      subroutine set_dynamic_elsasser_address                           &
     &         (els_dat, iels, nfield_sph_spec, ntot_sph_spec)
!
      type(sph_dyn_elsasser_data), intent(in) :: els_dat
      type(dyn_elsasser_address), intent(inout) :: iels
      integer(kind = kint), intent(inout) :: nfield_sph_spec
      integer(kind = kint), intent(inout) :: ntot_sph_spec
!
      integer(kind = kint) :: jcou, ifld
!
!
      jcou = 0
      ifld = 0
      if(els_dat%irms_KE .ge. 3) then
        call add_new_field_address(ifld, jcou, n_vector,                &
     &                             iels%ist_KEne)
        call add_new_field_address(ifld, jcou, n_scalar,                &
     &                             iels%ist_Re)
        call add_new_field_address(ifld, jcou, n_vector,                &
     &                             iels%ist_ulength_l)
        call add_new_field_address(ifld, jcou, n_vector,                &
     &                             iels%ist_ulength_m)
      end if
      if(els_dat%irms_ME .ge. 3) then
        call add_new_field_address(ifld, jcou, n_vector,                &
     &                             iels%ist_MEne)
        call add_new_field_address(ifld, jcou, n_scalar,                &
     &                             iels%ist_Elsasser)
        call add_new_field_address(ifld, jcou, n_vector,                &
     &                             iels%ist_Blength_l)
        call add_new_field_address(ifld, jcou, n_vector,                &
     &                             iels%ist_Blength_m)
      end if
      if(els_dat%irms_KE .ge. 3 .and. els_dat%irms_ME .ge. 3) then
        call add_new_field_address(ifld, jcou, n_scalar,                &
     &                             iels%ist_Rm)
        call add_new_field_address(ifld, jcou, n_scalar,                &
     &                             iels%ist_dyn_Els)
      end if
      if(els_dat%irms_T .ge. 1) then
        call add_new_field_address(ifld, jcou, n_scalar,                &
     &                             iels%ist_Temp)
        call add_new_field_address(ifld, jcou, n_scalar,                &
     &                             iels%ist_Tlength_l)
        call add_new_field_address(ifld, jcou, n_scalar,                &
     &                             iels%ist_Tlength_m)
      end if
      if(els_dat%irms_C .ge. 1) then
        call add_new_field_address(ifld, jcou, n_scalar,                &
     &                             iels%ist_Comp)
        call add_new_field_address(ifld, jcou, n_scalar,                &
     &                             iels%ist_Clength_l)
        call add_new_field_address(ifld, jcou, n_scalar,                &
     &                             iels%ist_Clength_m)
      end if
!
      nfield_sph_spec = ifld
      ntot_sph_spec =   jcou
!
      end subroutine set_dynamic_elsasser_address
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
      end module t_dyn_elsasser_address
