!drcap_pg.f90
!     module drcap_pg
!
!*************************************************
!*
!*  subroutine for draw captions
!*
!*************************************************
!*
!      subroutine drcap (npanel, iw, x_cap, title)
!      subroutine drcap_zplane(npanel, iw, z_position)
!      subroutine drcap_xplane(npanel, iw, x_position)
!      subroutine drcap_map_radius(npanel, iw, r_position)
!
      module drcap_pg
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine drcap (npanel, iw, x_cap, title)
!
      integer(kind= kint), intent(in) :: npanel, iw
      character(len=kchara), intent(in) :: title
      real(kind = kreal), intent(in) :: x_cap
!*
      character(len=32) :: cmtime
!*
!*  -------   setting of  comment of time ---------------------
!*
      write(cmtime,3020) x_cap
!*
!*  -------   write label of the graph ---------------------
!*
      call pgsci(1)
!*
!* +++++ graph title ++++++++++++++++
!*
      call pgsch(2.0)
      call pgmtxt('t',1.0, 0.5, 0.5, title )
!*
      if ( mod(iw,npanel) .eq. 1 ) then
!* +++++ dimension of simulation +++++
!*
      call pgsch(1.0)
!
!*  +++++++  time ++++++++++++++++
!*
      call pgsch(1.5)
!        call pgmtxt( 't', 0.3, 0.0, 0.0, cmtime )
      call pgsch(1.0)
!*
!*  +++++++ graphic mode ++++++++++++++++
!*
      end if
!*
 3020 format('\frtime=',f7.2)
!*
      end subroutine drcap
!
! ----------------------------------------------------------------------
!
      subroutine drcap_zplane(npanel, iw, z_position)
!
      integer(kind= kint), intent(in) :: npanel, iw
      real(kind = kreal), intent(in) :: z_position
!*
      character(len=32) :: cm_zposi
!*
!*  +++++++  position ++++++++++++++++
      if ( mod(iw,npanel) .eq. 1 ) then
        write(cm_zposi,3020) z_position
        call pgsci(1)
        call pgsch(1.5)
        call pgmtxt( 't', 0.3, 1.0, 1.0, cm_zposi )
      end if
!*
 3020 format('\frz =',f7.2)
!*
      end subroutine drcap_zplane
!
! ----------------------------------------------------------------------
!
      subroutine drcap_xplane(npanel, iw, x_position)
!
      integer(kind= kint), intent(in) :: npanel, iw
      real(kind = kreal), intent(in) :: x_position
!*
      character(len=32) :: cm_zposi
!*
!*  +++++++  position ++++++++++++++++
      if ( mod(iw,npanel) .eq. 1 ) then
        write(cm_zposi,3020) x_position
        call pgsci(1)
        call pgsch(1.5)
        call pgmtxt( 't', 0.3, 1.0, 1.0, cm_zposi )
      end if
!*
 3020 format('\frx =',f7.2)
!*
      end subroutine drcap_xplane
!
! ----------------------------------------------------------------------
!
      subroutine drcap_map_radius(npanel, iw, r_position)
!
      integer(kind= kint), intent(in) :: npanel, iw
      real(kind = kreal), intent(in) :: r_position
!*
      character(len=32) :: cm_rposi
!*
!*  +++++++  position ++++++++++++++++
      if ( mod(iw,npanel) .eq. 1 ) then
        write(cm_rposi,3020) r_position
        call pgsci(1)
        call pgsch(1.5)
        call pgmtxt( 't', 0.3, 1.0, 1.0, cm_rposi )
      end if
!*
 3020 format('\frr=',f7.2)
!*
      end subroutine drcap_map_radius
!
! ----------------------------------------------------------------------
!
      end module drcap_pg
