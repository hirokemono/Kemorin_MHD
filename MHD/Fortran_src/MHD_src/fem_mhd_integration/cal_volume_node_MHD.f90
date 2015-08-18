!
!------- module cal_volume_node_MHD ---------------
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        Modified by H. Matsui on Aug., 2006
!        Modified by H. Matsui on Aug., 2007
!
!      subroutine cal_volume_node
!
      module cal_volume_node_MHD
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use m_geometry_data
      use sum_volume_of_domain
!
       implicit none
!
      real(kind=kreal) :: vol_fl_local
      private :: vol_fl_local
!
      private :: cal_volume_4_fluid, cal_volume_4_conduct
      private :: cal_volume_4_insulate
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cal_volume_node
!
      use m_control_parameter
      use m_geometry_data_MHD
      use m_bulk_values
!
      use int_volume_of_domain
      use cal_layered_volumes
!
!
      call allocate_volume_4_smp
!
      if (iflag_debug.eq.1) write(*,*) 's_int_volume_of_domain'
      call s_int_volume_of_domain
!
!     ---  lead total volume of each area
!
      if (iflag_debug.eq.1) write(*,*) 'cal_volume_4_fluid'
       call cal_volume_4_fluid
      if (iflag_debug.eq.1) write(*,*) 'cal_volume_4_conduct'
       call cal_volume_4_conduct
      if (iflag_debug.eq.1) write(*,*) 'cal_volume_4_insulate'
       call cal_volume_4_insulate
!
       if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
         if (iflag_debug.eq.1) write(*,*) 's_cal_layered_volumes'
         call s_cal_layered_volumes
       end if
!
!
       if (iele_fl_smp_stack(np_smp) .eq. iele_fl_smp_stack(0)) then
         rms_local(ivol) = vol_local
       else
         rms_local(ivol) = vol_fl_local
       end if
!
!       call s_int_volume_insulate_core
!
       call deallocate_volume_4_smp
!
!
       if (iflag_debug.eq.1) then
         write(*,*) 'volume:       ', ele1%volume
         write(*,*) 'vol_fluid:    ', vol_fluid
         write(*,*) 'vol_conduct:  ', vol_conduct
         write(*,*) 'vol_insulate: ', vol_insulate
       end if
!
       end subroutine cal_volume_node
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_volume_4_fluid
!
      use m_geometry_data_MHD
!
      call sum_4_volume(ele1%numele, ele1%interior_ele,                 &
     &    iele_fl_smp_stack, ele1%volume_ele, vol_fl_local)
!
      call MPI_allREDUCE (vol_fl_local, vol_fluid, 1,                   &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      if (vol_fluid .eq. 0.0d0) then
        a_vol_fl = 1.0d30
      else
        a_vol_fl = 1.0d0 / vol_fluid
      end if
!
      end subroutine cal_volume_4_fluid
!
!  ---------------------------------------------------------------------
!
      subroutine cal_volume_4_conduct
!
      use m_geometry_data_MHD
!
      real(kind=kreal) :: vol_cd_local
!
!
      call sum_4_volume(ele1%numele, ele1%interior_ele,                 &
     &    iele_cd_smp_stack, ele1%volume_ele, vol_cd_local)
!
      call MPI_allREDUCE (vol_cd_local, vol_conduct, 1,                 &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      if (vol_conduct .eq. 0.0d0) then
        a_vol_cd = 1.0d30
      else
        a_vol_cd = 1.0d0 / vol_conduct
      end if
!
      end subroutine cal_volume_4_conduct
!
!  ---------------------------------------------------------------------
!
      subroutine cal_volume_4_insulate
!
      use m_geometry_data_MHD
!
      real(kind=kreal) :: vol_ins_local
!
!
      call sum_4_volume(ele1%numele, ele1%interior_ele,                 &
     &    iele_ins_smp_stack, ele1%volume_ele, vol_ins_local)
!
      call MPI_allREDUCE (vol_ins_local, vol_insulate, 1,               &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      if (vol_insulate .eq. 0.0d0) then
        a_vol_ins = 1.0d30
      else
        a_vol_ins = 1.0d0 / vol_insulate
      end if
!
      end subroutine cal_volume_4_insulate
!
!  ---------------------------------------------------------------------
!
      end module cal_volume_node_MHD
